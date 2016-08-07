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
        evil-org
        evil-surround
        evil-nerd-commenter
        ;; evil-leader
        flx-ido
        flycheck
        auctex
        latex-preview-pane
        ag
        company
        smex
        projectile
        markdown-mode+
        magit
        paredit
        yasnippet
        erlang
        yaml-mode
        dash-at-point
        multi-term
        terraform-mode
        nlinum-relative
        json-mode
        rainbow-delimiters

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

;; Mac specific stuff
;; (setq mac-command-modifier 'meta)
(toggle-frame-fullscreen)
(global-set-key (kbd "M-C-f") 'toggle-frame-fullscreen)

;; evil-mode
(require 'evil)
(require 'evil-surround)
;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader ",")
(setq evil-shift-width 4
      evil-search-module 'evil-search)
(evil-mode t)
(global-set-key (kbd "C-S-h") 'help)
(define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
(evil-ex-define-cmd "W" 'save-buffer)
(evil-ex-define-cmd "Q" 'save-buffers-kill-terminal)
(global-evil-surround-mode 1)

(evil-define-operator evil-delete-without-register (beg end type yank-handler)
  "Delete from beg to end and send to \"_ register"
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))

(evil-define-operator evil-stamp (beg end)
  "Replace text-object with 0th register contents"
  (evil-delete-without-register beg end)
  (evil-paste-from-register ?0))

(define-key evil-normal-state-map (kbd "S") 'evil-stamp)


;; key-bindings
(global-set-key (kbd "C-c g") 'ag)
(global-set-key (kbd "C-c a") 'align-regexp)

;; Variables
(setq make-backup-files nil)
(setq-default
 inhibit-splash-screen t
 tab-width 4
 indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; Mode toggles
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-font-lock-mode t)
;; (global-hl-line-mode t)


;; shell
(require 'multi-term)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-c s") 'multi-term)
(global-set-key (kbd "C-c $") 'eshell)
(add-hook
 'term-mode-hook
 (lambda ()
   (setq term-buffer-maximum-size 10000)
   (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
   (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
   (setq yas-dont-activate t)))
;; (multi-term)


;; smex / ido
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(setq-default
 ido-enable-flex-matching t
 ;; ido-use-faces nil
 ido-max-window-height 1)


;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-ex-define-cmd "ls" 'ibuffer)

(defun ibuffer-generate-filter-groups-by-major-mode ()
  (flet
      ((mode-group
        (mode)
        (let ((mode-title
               (capitalize (car (split-string (symbol-name mode) "-" t)))))
          (cons mode-title `((mode . ,mode)))))
       (buffer-modes
        ()
        (flet ((buffer-mode (buffer) (buffer-local-value 'major-mode buffer)))
          (ibuffer-remove-duplicates (remq nil (mapcar 'buffer-mode (buffer-list)))))))
    (mapcar 'mode-group (buffer-modes))))

(defun ibuffer-major-mode-group-hook ()
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-generate-filter-groups-by-major-mode))
  (ibuffer-update nil t)
  (message "ibuffer-major-mode: groups set"))

(setq-default ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-hook (lambda () (ibuffer-major-mode-group-hook)))


;; line numbers
;; (global-linum-mode t)
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(global-nlinum-relative-mode)
;; (add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)
(setq nlinum-relative-current-symbol "")
(setq nlinum-relative-offset 0) 


;; Paren
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Font
;; (setq mac-allow-anti-aliasing nil)
;; (set-frame-font "Terminus (TTF)-14")
;; (set-face-attribute 'default nil :family "Terminus (TTF)" :height 160)
(set-frame-font "Inconsolata-14")

;; Theme
(setq
 solarized-use-variable-pitch nil
 solarized-scale-org-headlines nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("99953b61ecd4c3e414a177934e888ce9ee12782bbaf2125ec2385d5fd732cbc2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "113ae6902d98261317b5507e55ac6e7758af81fc4660c34130490252640224a2" "d76af04d97252fafacedc7860f862f60d61fdcfbd026aeba90f8d07d8da51375" "01d8c9140c20e459dcc18addb6faebd7803f7d6c46d626c7966d3f18284c4502" "3328e7238e0f6d0a5e1793539dfe55c2685f24b6cdff099c9a0c185b71fbfff9" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "3f78849e36a0a457ad71c1bda01001e3e197fe1837cb6eaa829eb37f0a4bdad5" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (rainbow-delimiters json-mode evil-nerd-commenter sr-speedbar latex-preview-pane ansible-doc company-ansible jinja2-mode haskell-mode yasnippet company flycheck evil yaml-mode w3m ujelly-theme twilight-theme terraform-mode solarized-theme smex projectile paredit nlinum-relative multi-term moe-theme markdown-mode+ magit intero hindent haskell-snippets flycheck-elm flx-ido exec-path-from-shell evil-surround evil-org erlang elm-mode dash-at-point base16-theme auctex ag))))

;; (if window-system
;;     (load-theme 'base16-tomorrow-dark))

;; (require 'moe-theme)
;; (setq moe-theme-highlight-buffer-id t) ; moe-theme settings
;; (setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
;; (setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
;; (setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
;; (moe-dark)

(load-theme 'solarized-light)

;; Fundamental
(add-hook 'fundamental-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default
 company-show-numbers t
;;  company-idle-delay nil
;;  company-minimum-prefix-length 2
;;  company-selection-wrap-around t
;;  company-tooltip-align-annotations t)
 )

;; markdown-mode - What about markdown-mode+?
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(setq-default
 markdown-command "pandoc -f markdown_github")

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)

;; yasnippets
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)) ; use ido for multiple snippets
(yas-global-mode t)

;; dash-at-point
(autoload 'dash-at-point "dash-at-point" "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;; postgresql sqli
(evil-set-initial-state 'sql-interactive-mode 'emacs)

;; js indent level
(setq js-indent-level 2)

;; tramp
(require 'tramp)
(setq tramp-default-method "sshx")

;; rainbow delimiters
(rainbow-delimiters-mode t)

;; Haskell
;; (require 'haskell-init)
(require 'intero-init)

;; OCaml
(require 'ocaml-init)

;; Erlang
(require 'erlang-start)

;; Elm
(require 'elm-init)

;; Ansible
(require 'ansible-init)

;; org-mode
(require 'org-init)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
