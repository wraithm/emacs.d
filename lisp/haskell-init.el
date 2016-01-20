;; haskell-init
;; Matthew Wraith <wraithm@gmail.com>

;; Install packages
(setq my-haskell-packages
      '(haskell-mode
        haskell-snippets

        flycheck
        ;; flycheck-ghcmod ;; Which one to use?
        flycheck-haskell ;; Maybe bad?

        company-ghc

        ;; hi2
        hindent))
(mapc #'package-install my-haskell-packages)

;; stack-mode
;; (add-to-list 'load-path "~/.emacs.d/stack-ide/stack-mode")

;; company-stack-ide
;; (add-to-list 'load-path "~/.emacs.d/stack-ide/company-stack-ide")
;; (require 'company-stack-ide)
;; (add-to-list 'company-backends 'company-stack-ide)
;; (add-hook 'stack-mode 'company-mode)

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-snippets)
(require 'haskell-interactive-mode)
(require 'flycheck)
;; (require 'flycheck-ghcmod)
(require 'flycheck-haskell)
;; (require 'hi2)
;; (require 'stack-mode)
(require 'company-ghc)

(add-hook
 'flycheck-mode-hook
 #'flycheck-haskell-setup)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec ag -i --numbers %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

;; Other plugins
(add-hook
 'haskell-mode-hook
 (lambda ()
   ;; (subword-mode)
   (turn-on-haskell-indentation)
   (ghc-init)
   (flycheck-mode)
   ;; (turn-on-hi2)
   ;; (stack-mode)
   (electric-indent-local-mode -1))

 ;; Variables
 (setq
  haskell-process-auto-import-loaded-modules t
  haskell-process-log t
  haskell-process-show-debug-tips nil
  haskell-process-suggest-remove-import-lines t
  haskell-process-type 'stack-ghci
  ;; haskell-process-type 'cabal-repl
  ;; haskell-process-use-presentation-mode t

  haskell-interactive-mode-eval-pretty t
  haskell-interactive-mode-scroll-to-bottom t
  haskell-interactive-mode-eval-mode 'haskell-mode

  haskell-indentation-layout-offset 4
  haskell-indentation-left-offset 4
  haskell-indentation-ifte-offset 4
  haskell-indentation-show-indentations t
  haskell-indentation-show-indentations-after-eol t
  ;; haskell-indent-offset 4
  ;; hi2-ifte-offset 4
  ;; hi2-layout-offset 4
  ;; hi2-left-offset 4

  haskell-stylish-on-save t))

(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

(add-hook
 'haskell-cabal-mode-hook
 (setq haskell-cabal-list-comma-position 'after))

;; hindent
(setq hindent-style "johan-tibell")
(add-hook 'haskell-mode-hook #'hindent-mode) 

;; Key bindings
(global-set-key (kbd "M-g M-f") 'first-error)

(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "C-c C-d") 'haskell-describe)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "M-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-r") 'hindent/reformat-decl)

(define-key haskell-cabal-mode-map (kbd "M-`") 'haskell-interactive-bring)

(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-process-do-type)

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "M-.") nil)
(define-key evil-insert-state-map (kbd "M-.") nil)
(define-key evil-emacs-state-map (kbd "M-.") nil)
;; (define-key stack-mode-map (kbd "M-.") 'stack-mode-goto)

;; (evil-leader/set-key-for-mode 'stack-mode-map "t" 'stack-mode-type)
;; (evil-leader/set-key-for-mode 'stack-mode-map "i" 'stack-mode-info)

;; (evil-leader/set-key-for-mode 'haskell-mode-map "y" 'haskell-mode-stylish-buffer)

(evil-set-initial-state 'haskell-interactive-mode 'emacs)

;; Alignment
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

(global-set-key (kbd "C-x a r") 'align-regexp)

;; (defun evil-open-below (count)
;;   "Insert a new line below point and switch to Insert state.
;; The insertion will be repeated COUNT times."
;;   (interactive "p")
;;   (evil-insert-newline-below)
;;   (setq evil-insert-count count
;;         evil-insert-lines t
;;         evil-insert-vcount nil)
;;   (evil-insert-state 1)
;;   (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

(message "Loading haskell-init... Done.")
(provide 'haskell-init)
