(defvar my-evil-packages
  '(evil
    evil-leader
    evil-surround
    evil-escape
    evil-org
    evil-nerd-commenter
    evil-ediff))
(mapc #'package-install my-evil-packages)

;; evil-mode
(require 'evil-vars)
(setq evil-want-C-u-scroll t) ; must come before (require 'evil) call
(require 'evil)
(require 'evil-surround)
(setq evil-shift-width 4
      evil-search-module 'evil-search
      evil-want-C-w-in-emacs-state t)
(evil-mode t)
(global-evil-surround-mode t)

(require 'evil-escape)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
(evil-escape-mode)

;; Patch for evil-escape
(defun ibuffer-quit ()
  "Quit ibuffer window."
  (interactive)
  (quit-window))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "g" 'ag
  "G" 'projectile-ag
  "l" 'ibuffer
  "L" 'projectile-ibuffer
  "f" 'find-file
  "p" 'projectile-find-file
  "e" 'first-error
  "n" 'next-error
  "c" 'compile
  "r" 'recompile
  "w" 'save-buffer
  "k" 'kill-buffer-and-window
  "a" 'align-regexp)

;; key-bindings
(global-set-key (kbd "C-c g") 'ag)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "M-g M-f") 'first-error)

(global-set-key (kbd "C-S-h") 'help)
(define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-motion-state-map (kbd "C-w C-h") 'undefined)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "gs") 'ff-find-other-file) ; TODO This doesn't work?
(define-key evil-normal-state-map "U" 'universal-argument)
(define-key evil-motion-state-map "U" 'universal-argument)

(require 'evil-nerd-commenter)
(require 'evil-nerd-commenter-operator)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
;; (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

(fset 'evil-visual-update-x-selection 'ignore)
(defun evil-set-paste ()
  (interactive)
  (setq select-enable-clipboard t))
(defun evil-unset-paste ()
  (interactive)
  (setq select-enable-clipboard nil))
(evil-ex-define-cmd "paste" 'evil-set-paste)
(evil-ex-define-cmd "unpaste" 'evil-unset-paste)
;; (evil-unset-paste) ; If I want to have pasting disabled at startup

(evil-ex-define-cmd "W" 'save-buffer)
(evil-ex-define-cmd "Q" 'save-buffers-kill-terminal)
(evil-ex-define-cmd "BD" 'kill-this-buffer)

;; Scrolling
(setq scroll-step 1
      hscroll-step 1
      scroll-margin 1
      scroll-conservatively 9999)

;; Stamp operator
(evil-define-operator evil-delete-without-register (beg end type yank-handler)
  "Delete from beg to end and send to \"_ register"
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))

(evil-define-operator evil-stamp (beg end)
  "Replace text-object with 0th register contents"
  (evil-delete-without-register beg end)
  (evil-paste-from-register ?0))

(define-key evil-normal-state-map (kbd "S") 'evil-stamp)


(provide 'init-evil)
