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

(defun haskell-company-backends ()
  (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))

(defun stack-compile-command ()
  (interactive)
  (setq-local compile-command "stack build -j4 --test --bench --no-run-tests --no-run-benchmarks --no-interleaved-output"))

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . haskell-company-backends)
         (haskell-mode . stack-compile-command)
         (haskell-mode . yas-minor-mode)
         (haskell-cabal-mode . stack-compile-command))

  :bind (("C-c C-t" . haskell-mode-show-type-at)
         ("C-]" . haskell-mode-jump-to-def-or-tag)
         ("C-c C-l" . haskell-process-load-file)
         ("C-`" . haskell-interactive-bring)
         ;; ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-r" . haskell-process-restart)
         ("C-c C" . haskell-process-cabal-build)
         ("M-n" . haskell-goto-next-error)
         ("M-p" . haskell-goto-prev-error)
         ("C-c M-p" . haskell-goto-prev-error)
         :map haskell-cabal-mode-map
         ("C-c C-c" . haskell-compile)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C" . haskell-process-cabal-build))

  :config
  (setq haskell-stylish-on-save t
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-compile-cabal-build-command "stack build -j4 --test --bench --no-run-tests --no-run-benchmarks --no-interleaved-output"
        haskell-compile-cabal-build-alt-command (concat "stack clean && " haskell-compile-cabal-build-command)
        haskell-process-type 'stack-ghci
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
  (evil-leader/set-key-for-mode 'haskell-mode "r" 'haskell-process-restart)

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


(message "Loading init-haskell...")
(provide 'init-haskell)
