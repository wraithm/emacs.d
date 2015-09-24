;; haskell-init
;; Matthew Wraith <wraithm@gmail.com>

;; Install packages
(setq my-haskell-packages
      '(haskell-mode
        haskell-snippets
        ;; hi2
        hindent))
(mapc #'package-install my-haskell-packages)

;; stack-mode
(add-to-list 'load-path "~/.emacs.d/stack-ide/stack-mode")

;; company-stack-ide
(add-to-list 'load-path "~/.emacs.d/stack-ide/company-stack-ide")
(require 'company-stack-ide)
(add-to-list 'company-backends 'company-stack-ide)
(add-hook 'stack-mode 'company-mode)

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-snippets)
(require 'haskell-interactive-mode)
(require 'stack-mode)
;; (require 'haskell-process)
;; (require 'flycheck)
;; (require 'hi2)


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
   (subword-mode)
   (turn-on-haskell-indentation)
   (haskell-indentation-enable-show-indentations)
   ;; (turn-on-hi2)
   (stack-mode)
   (electric-indent-mode nil)
   (interactive-haskell-mode t))

  ;; Variables
  (setq
   haskell-process-auto-import-loaded-modules t
   haskell-process-log t
   haskell-process-show-debug-tips nil
   haskell-process-suggest-remove-import-lines t
   haskell-process-type 'stack-ghci
   ;; haskell-process-type (quote cabal-repl)
   ;; haskell-process-use-presentation-mode t

   haskell-interactive-mode-eval-pretty t
   haskell-interactive-mode-scroll-to-bottom t
   haskell-interactive-mode-eval-mode 'haskell-mode

   haskell-indentation-layout-offset 4
   haskell-indentation-left-offset 4

   haskell-stylish-on-save t))

;; hindent
(setq hindent-style "johan-tibell")
(add-hook 'haskell-mode-hook #'hindent-mode) 

;; Key bindings
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
(define-key stack-mode-map (kbd "M-.") 'stack-mode-goto)

(evil-leader/set-key-for-mode 'stack-mode-map "t" 'stack-mode-type)
(evil-leader/set-key-for-mode 'stack-mode-map "i" 'stack-mode-info)
(evil-leader/set-key-for-mode 'haskell-mode-map "y" 'haskell-mode-stylish-buffer)

(evil-set-initial-state 'haskell-interactive-mode 'emacs)


(message "Loading haskell-init... Done.")
(provide 'haskell-init)
