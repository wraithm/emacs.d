;; Packages
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
    '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives
;;   '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; Download packages
(setq my-packages
      '(exec-path-from-shell
        evil
        evil-leader
        evil-org
        evil-nerd-commenter
        flx-ido
        flycheck
        auctex
        ag
        powerline
        powerline-evil
        company
        smex
        projectile
        markdown-mode+
        magit
        paredit
        yasnippet

        ;; Themes
        solarized-theme
        moe-theme
        twilight-theme
        ujelly-theme
        base16-theme))
(mapc #'package-install my-packages)

;; My custom code
(add-to-list 'load-path "~/.emacs.d/lisp")

;; PATH
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; evil-mode
(require 'evil)
(evil-mode 1)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(global-set-key (kbd "C-S-h") 'help)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

;; smex / ido
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(setq-default
 ido-enable-flex-matching t
 ido-use-faces nil
 ido-max-window-height 1)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default
 ibuffer-show-empty-filter-groups nil
 ibuffer-save-filter-groups
 '(("default"
    ("Emacs Lisp" (mode . emacs-lisp-mode))
    ("Haskell" (mode . haskell-mode))
    ("Cabal" (mode . haskell-cabal-mode)))))
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   (ido-mode t)
   (ibuffer-switch-to-saved-filter-groups "default")))

;; Variables
(setq make-backup-files nil)
(setq-default
 inhibit-splash-screen t
 indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Mode toggles
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode t)

;; Paren
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Font
;; (set-frame-font "Terminus (TTF)-12")
(set-face-attribute 'default nil :family "Terminus (TTF)" :height 140)


;; Theme
(setq
 solarized-use-variable-pitch nil
 solarized-scale-org-headlines nil)
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "113ae6902d98261317b5507e55ac6e7758af81fc4660c34130490252640224a2"
     "d76af04d97252fafacedc7860f862f60d61fdcfbd026aeba90f8d07d8da51375"
     "01d8c9140c20e459dcc18addb6faebd7803f7d6c46d626c7966d3f18284c4502"
     "3328e7238e0f6d0a5e1793539dfe55c2685f24b6cdff099c9a0c185b71fbfff9"
     "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149"
     "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d"
     "3f78849e36a0a457ad71c1bda01001e3e197fe1837cb6eaa829eb37f0a4bdad5"
     "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2"
     "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     default))))

;; (if window-system
;;     (load-theme 'base16-tomorrow-dark))

(require 'powerline)
(require 'moe-theme)
(require 'powerline-evil-moe) ; My hack powerline moe-theme

(setq moe-theme-highlight-buffer-id t) ; moe-theme settings
(setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))

(powerline-moe-theme)
(moe-dark)

;; Fundamental
(add-hook 'fundamental-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)

;; Org
(require 'evil-org)
(add-hook
 'org-mode-hook
 (lambda ()
   (face-remap-add-relative 'default :family "Terminus (TTF)-12")
   (turn-on-auto-fill)))
(setq org-todo-keywords
      '((sequence "TODO" "WORK" "DONE")))

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default
 company-idle-delay nil
 company-minimum-prefix-length 2
 company-selection-wrap-around t
 company-show-numbers t
 company-tooltip-align-annotations t)

;; markdown-mode
(add-hook 'markdown-mode-hook 'flyspell-mode)
(setq-default
 markdown-command "pandoc -f markdown_github")

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;;; yasnippets
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)) ; use ido for multiple snippets
(yas-global-mode t)

;; Haskell
(require 'haskell-init)
