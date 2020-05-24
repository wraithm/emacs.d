(require 'company)
(require 'evil)
(require 'evil-leader)
(require 'align)
(require 'haskell)
(require 'haskell-mode)
(require 'haskell-compile)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(require 'flycheck)

(use-package company-lsp
  :ensure t)

(defun haskell-company-backends ()
  (set (make-local-variable 'company-backends)
                   (append '((company-lsp company-capf company-dabbrev-code))
                           company-backends)))

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . haskell-company-backends)
         (haskell-mode . yas-minor-mode))

  :bind (("C-c C-t" . haskell-mode-show-type-at)
         ("C-]" . haskell-mode-jump-to-def-or-tag)
         ("C-c C-l" . haskell-process-load-file)
         ("C-`" . haskell-interactive-bring)
         ;; ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-r" . haskell-process-restart)
         ("C-c C" . haskell-process-cabal-build)
         ("M-n" . haskell-goto-next-error) ; TODO create macro to go to error with flycheck first and haskell mode second
         ("M-p" . haskell-goto-prev-error)
         ("C-c M-p" . haskell-goto-prev-error)
         :map haskell-cabal-mode-map
         ("C-c C-c" . haskell-compile)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C" . haskell-process-cabal-build))

  :config
  (setq haskell-stylish-on-save t
        haskell-interactive-set-+c t
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-compile-cabal-build-command "cabal build --ghc-option=-ferror-spans all"
        haskell-compile-cabal-build-alt-command (concat "cabal clean && " haskell-compile-cabal-build-command)
        haskell-process-type 'cabal-repl
        haskell-process-suggest-remove-import-lines t
        haskell-process-suggest-hoogle-imports t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)

  (evil-define-motion my-haskell-navigate-imports ()
    "Navigate imports with evil motion"
    :jump t
    :type line
    (haskell-navigate-imports))

  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)

  (evil-leader/set-key-for-mode 'haskell-mode "h" 'hoogle)
  (evil-leader/set-key-for-mode 'haskell-mode "i" 'my-haskell-navigate-imports)
  (evil-leader/set-key-for-mode 'haskell-mode "t" 'haskell-mode-show-type-at)
  ;; (evil-leader/set-key-for-mode 'haskell-mode "r" 'haskell-process-restart)

  (eval-after-load "align"
    '(add-to-list 'align-rules-list
                  '(haskell-types
                    (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                    (modes quote (haskell-mode literate-haskell-mode)))))
  (eval-after-load "align"
    '(add-to-list 'align-rules-list
                  '(haskell-assignment
                    (regexp . "\\(\\s-+\\)=\\s-+")
                    (modes quote (haskell-mode literate-haskell-mode)))))
  (eval-after-load "align"
    '(add-to-list 'align-rules-list
                  '(haskell-arrows
                    (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                    (modes quote (haskell-mode literate-haskell-mode)))))
  (eval-after-load "align"
    '(add-to-list 'align-rules-list
                  '(haskell-left-arrows
                    (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                    (modes quote (haskell-mode literate-haskell-mode)))))

  (defun haskell-indentation-indent-line ()
    "Indent current line, cycle though indentation positions.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
    (interactive)
    ;; try to repeat
    (when (not (haskell-indentation-indent-line-repeat))
      (setq haskell-indentation-dyn-last-direction nil)
      ;; parse error is intentionally not caught here, it may come from
      ;; `haskell-indentation-find-indentations', but escapes the scope
      ;; and aborts the operation before any moving happens
      (let* ((cc (current-column))
             (ci (haskell-indentation-current-indentation))
             (inds (save-excursion
                     (move-to-column ci)
                     (or (haskell-indentation-find-indentations)
                         '(0))))
             (valid (memq ci inds))
             (cursor-in-whitespace (< cc ci))
             ;; certain evil commands need the behaviour seen in
             ;; `haskell-indentation-newline-and-indent'
             (evil-special-command (and (bound-and-true-p evil-mode)
                                        (memq this-command '(evil-open-above
                                                             evil-open-below
                                                             evil-replace))))
             (on-last-indent (eq ci (car (last inds)))))
        (if (and valid cursor-in-whitespace)
            (move-to-column ci)
          (haskell-indentation-reindent-to
           (funcall
            (if on-last-indent
                #'haskell-indentation-previous-indentation
              #'haskell-indentation-next-indentation)
            (if evil-special-command
                (save-excursion
                  (end-of-line 0)
                  (1- (haskell-indentation-current-indentation)))
              ci)
            inds
            'nofail)
           cursor-in-whitespace))
        (setq haskell-indentation-dyn-last-direction (if on-last-indent 'left 'right)
              haskell-indentation-dyn-last-indentations inds))))

  )

(projectile-register-project-type 'haskell-cabal '("cabal.project")
                  :compile "cabal build --ghc-option=-ferror-spans -j all"
                  :test "cabal test -j --keep-going all"
                  ;; :run "cabal run bitnomial-local-whatever"
                  ;; :test-suffix ".hs")
                  )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((lsp-managed-mode . lsp-diagnostics-modeline-mode))
  :config
  (setq lsp-diagnostic-package :flycheck
        lsp-auto-guess-root t
        lsp-diagnostics-modeline-scope :project
        lsp-enable-xref nil)

  (evil-leader/set-key
    "q" 'lsp-ui-flycheck-list)

  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq warning-minimum-level ':error) ; This is temporary for a bug in lsp-ui that pops up errors
  (setq lsp-ui-sideline-enable nil)
  )

(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "hie-wrapper")

 (add-hook 'haskell-mode-hook
            (lambda ()
              (lsp)
              (lsp-flycheck-enable t)
              (lsp-ui-sideline-enable nil)))

 ;; For ghcide
 ;; (setq lsp-haskell-process-path-hie "ghcide"
 ;;       lsp-haskell-process-args-hie '())

 (lsp-haskell-set-liquid-off)
 (lsp-haskell-set-hlint-on)

 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;; (setq lsp-log-io t)
 )

(message "Loading init-haskell...")
(provide 'init-haskell-lsp-cabal)
