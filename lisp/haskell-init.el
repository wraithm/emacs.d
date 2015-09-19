;; haskell-init
;; Matthew Wraith <wraithm@gmail.com>

;; stack-mode
(add-to-list 'load-path "~/.emacs.d/stack-ide/stack-mode")

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-snippets)
(require 'haskell-interactive-mode)
(require 'stack-mode)
;; (require 'shm)
;; (require 'shm-case-split)
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
   ;; (turn-on-hi2)
   (stack-mode)
   (electric-indent-mode nil)
   ;; (structured-haskell-mode t)
   (interactive-haskell-mode t)))

;; Variables
(custom-set-variables
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-show-debug-tips nil)
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-type 'stack-ghci)
  ;; '(haskell-process-type (quote cabal-repl))
  ;; '(haskell-process-use-presentation-mode t)

  '(haskell-interactive-mode-eval-pretty t)
  '(haskell-interactive-mode-scroll-to-bottom t)
  '(haskell-interactive-mode-eval-mode 'haskell-mode)

  '(haskell-indentation-layout-offset 4)
  '(haskell-indentation-left-offset 4)

  ;; '(shm-use-presentation-mode t)
  )

;; Key bindings
(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "C-c C-d") 'haskell-describe)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "M-`") 'haskell-interactive-bring)
;; (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-process-clear)
;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key haskell-cabal-mode-map (kbd "M-`") 'haskell-interactive-bring)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-process-do-type)

;; evil
;; (defun evil-open-below (count)
;;   "Insert a newline below point and switch to Insert state. The insertion will be repeated COUNT times."
;;   (interactive "p")
;;   (evil-insert-newline-below)
;;   (setq evil-insert-count count
;;         evil-insert-lines t
;;         evil-insert-vcount nil)
;;   (evil-insert-state 1)
;;   (add-hook 'post-command-hook #'evil-maybe-remove-spaces))


;; (define-key shm-map (kbd "C-c C-p") 'shm/expand-pattern)
;; (define-key shm-map (kbd "C-c C-s") 'shm/case-split)
;; (define-key shm-map (kbd "C-c C-p") 'shm/goto-last-point)
;; (define-key shm-map (kbd "C-S-J") 'shm/newline-indent-proxy)

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "M-.") nil)
(define-key evil-insert-state-map (kbd "M-.") nil)
(define-key evil-emacs-state-map (kbd "M-.") nil)
(define-key stack-mode-map (kbd "M-.") 'stack-mode-goto)
;; (define-key map (kbd "C-c C-k") 'stack-mode-clear)
;; (define-key map (kbd "C-c C-t") 'stack-mode-type)
;; (define-key map (kbd "C-c C-i") 'stack-mode-info)
;; (define-key map (kbd "C-c C-l") 'stack-mode-load)
(evil-leader/set-key-for-mode 'stack-mode-map "t" 'stack-mode-type)
(evil-leader/set-key-for-mode 'stack-mode-map "i" 'stack-mode-info)


(message "Loading haskell-init... Done.")
(provide 'haskell-init)
