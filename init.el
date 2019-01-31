;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(use-package diminish) ; for use with use-package

;; Download packages
(defvar my-packages
  '(yasnippet
    flycheck
    company
    projectile
    ;; nlinum-relative
    ;; rainbow-delimiters

    multi-term
    w3m
    dash-at-point
    vagrant-tramp
    ix

    magit
    monky

    terraform-mode
    yaml-mode
    json-mode
    logstash-conf
    wolfram
    ansible
    web-mode
    gnuplot-mode
    auctex
    latex-preview-pane

    ;; erlang

    ;; Themes
    ;; moe-theme
    solarized-theme
    zenburn-theme
    twilight-theme
    ujelly-theme
    base16-theme))
(dolist (package my-packages)
 (unless (package-installed-p package)
   (package-install package)))

;; My custom code
(add-to-list 'load-path "~/.emacs.d/lisp")

;; PATH
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(require 'init-evil)

;; line numbers
(global-linum-mode t)
;; (require 'nlinum-relative)
;; (nlinum-relative-setup-evil)
;; (global-nlinum-relative-mode)
;; (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;; (setq nlinum-relative-redisplay-delay 0)
;; (setq nlinum-relative-current-symbol "")
;; (setq nlinum-relative-offset 0)

;; Highlight mode-line instead of audible bell
(defvar ring-bell-mode-line-color "#F2804F")
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line ring-bell-mode-line-color)
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Variables
(setq make-backup-files nil)
(setq-default
 inhibit-splash-screen t
 tab-width 4
 c-basic-offset 4
 cperl-indent-level 4
 indent-tabs-mode nil
 fill-column 120)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
;; (define-key global-map (kbd "RET") 'newline-and-indent)

(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq vc-handled-backends nil) ; to disable vc-mode entirely
;; (setq vc-handled-backends '(Hg)) ; Git))

;; Mode toggles
(menu-bar-mode -1)
(setq menu-bar-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-font-lock-mode t)
;; (global-hl-line-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun no-menubar-frame (&optional frame)
  "Do not display the menubar in FRAME (default: selected frame)."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines 0))
(add-hook 'after-make-frame-functions 'no-menubar-frame)

;; Mac specific stuff
;; (setq mac-command-modifier 'meta)
;; (toggle-frame-fullscreen)
(global-set-key (kbd "M-C-f") 'toggle-frame-fullscreen)
(when (eq system-type 'darwin)
  (advice-add 'ns-new-frame :after '(scroll-bar-mode -1))
  (advice-add 'ns-new-frame :after #'toggle-frame-fullscreen))

;; Fundamental
(add-hook 'fundamental-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)

;; unicode-fonts
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;; ag
(use-package ag)

;; flycheck
(use-package flycheck
  :init
  (setq-default
   flycheck-disabled-checkers '(emacs-lisp-checkdoc)
   flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

;; Browser
(use-package browse-url
  :init
  (setq browse-url-new-window-flag t))

;; yasnippets
(require 'yasnippet)
(yas-global-mode t)

;; shell
(require 'multi-term)
(require 'eshell)
(require 'em-term)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-c $") 'multi-term)
(global-set-key (kbd "C-c s") 'eshell)
(add-hook
 'term-mode-hook
 (lambda ()
   (nlinum-mode -1)
   (linum-mode -1)
   (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
   (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
   (setq yas-dont-activate-functions t)))
(setq term-buffer-maximum-size 10000)
(setenv "HGEDITOR" "emacsclient")
(add-hook
 'eshell-mode-hook
 (lambda ()
   (nlinum-mode -1)
   (linum-mode -1)
   (add-to-list 'eshell-visual-commands "htop")
   (setenv "TERM" "emacs")))
(setq eshell-visual-subcommands
      '(("hg" "di" "log" "glog")))
;; (multi-term)

;; ibuffer
(require 'ibuffer)
(require 'ibuf-ext)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(evil-ex-define-cmd "ls" 'ibuffer)

;; ibuffer group by major mode
(defun ibuffer-buffer-mode (buffer) (buffer-local-value 'major-mode buffer))
(defun ibuffer-buffer-modes ()
    (ibuffer-remove-duplicates (remq nil (mapcar 'ibuffer-buffer-mode (buffer-list)))))
(defun ibuffer-mode-group (mode)
  (let ((mode-title (capitalize (car (split-string (symbol-name mode) "-" t)))))
    (cons mode-title `((mode . ,mode)))))
(defun ibuffer-generate-filter-groups-by-major-mode ()
  (mapcar 'ibuffer-mode-group (ibuffer-buffer-modes)))

(defun ibuffer-major-mode-group-hook ()
  (interactive)
  (nlinum-mode -1)
  (linum-mode -1)
  (setq ibuffer-filter-groups (ibuffer-generate-filter-groups-by-major-mode))
  (ibuffer-update nil t)
  (message "ibuffer-major-mode: groups set"))

(setq-default ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-hook 'ibuffer-major-mode-group-hook)

;; ediff
(require 'ediff)
(defun my-kill-ediff-buffers ()
  (kill-buffer ediff-buffer-A)
  (kill-buffer ediff-buffer-B)
  (kill-buffer ediff-buffer-C))
(add-hook 'ediff-quit-hook 'my-kill-ediff-buffers)

;; Compilation
(require 'compile)
(setq compilation-scroll-output t)
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               ;; (window-height . 25)
               ;; display-buffer-pop-up-window
               ;; display-buffer-at-bottom

               display-buffer-reuse-window
               (reusable-frames . t)
               (inhibit-switch-frame . t)
               ))
;; (defun bury-compile-buffer-if-successful (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings."
;;   (if (and
;;        (string-match "compilation" (buffer-name buffer))
;;        (string-match "finished" string)
;;        (not
;;         (with-current-buffer buffer
;;           (goto-char (point-min))
;;           (search-forward "warning" nil t))))
;;       (run-with-timer 1 nil
;;                       (lambda (buf)
;;                         (bury-buffer buf)
;;                         (delete-window (get-buffer-window buf)))
;;                       buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Paren
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default
 company-show-numbers t
 company-idle-delay 0.2
;;  company-minimum-prefix-length 2
;;  company-selection-wrap-around t
;;  company-tooltip-align-annotations t)
 )
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
(defun abort-company-on-insert-state-exit () (company-abort))
(add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)
(define-key company-mode-map [remap hippie-expand] 'company-complete)
(define-key company-active-map [remap hippie-expand] 'company-complete)
(defun my-company-complete (arg) (company-complete))
(setq evil-complete-next-func 'my-company-complete
      evil-complete-previous-func 'my-company-complete)
(define-key evil-insert-state-map (kbd "C-x C-o") 'company-complete)

;; markdown-mode
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(setq-default markdown-command "pandoc -f markdown_github")

(require 'org-table)
(defun md-table-align ()
  (interactive)
  (org-table-align)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

;; projectile
(require 'projectile)
(projectile-mode)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-c C-b") 'projectile-ibuffer)
(global-set-key (kbd "C-c b") 'projectile-switch-to-buffer)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; dash-at-point
(autoload 'dash-at-point "dash-at-point" "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c e") 'dash-at-point-with-docset)

;; rcirc
(require 'rcirc)
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
(defun rcirc-detach-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
           (eq (process-status (rcirc-buffer-process)) 'open))
      (with-rcirc-server-buffer
    (setq rcirc-buffer-alist
          (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
      (rcirc-send-string (rcirc-buffer-process)
                 (concat "DETACH " rcirc-target))))
    (setq rcirc-target nil)
    (kill-buffer buffer)))
(define-key rcirc-mode-map (kbd "C-c C-d") 'rcirc-detach-buffer)

(load "~/.emacs.d/irc.el")

;; wolfram alpha
(load "~/.emacs.d/wolframalpha.el")

;; web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; tramp
(require 'tramp)
(setq tramp-default-method "sshx")

;; rainbow delimiters
;; (rainbow-delimiters-mode t)

;; narrowing and fuzzy match
(require 'init-ivy)
;; (rquire 'init-idosmex)

;; Haskell
;; (require 'init-haskell)
(require 'init-intero)

;; Rust
(require 'init-rust)

;; OCaml
;; (require 'init-ocaml)

;; Erlang
;; (require 'erlang-start)

;; Elm
;; (require 'init-elm)

;; Ansible
(require 'init-ansible)

;; org-mode
(require 'init-org)

;; ix
(require 'init-ix)

;; Javascript
(require 'init-javascript)

;; diminish
(require 'diminish)
(diminish 'evil-escape-mode) ; use-package.
(diminish 'yas-minor-mode) ; use-package.
(diminish 'undo-tree-mode) ; use-package?
(diminish 'company-mode) ; use-package.
(diminish 'eldoc-mode) ; use-package?

;; Don't touch this stuff below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ce3e6c12b48979ce89754884d913c7ecc8a7956543d8b09ef13abfab6af9aa35" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "be4025b1954e4ac2a6d584ccfa7141334ddd78423399447b96b6fa582f206194" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "99953b61ecd4c3e414a177934e888ce9ee12782bbaf2125ec2385d5fd732cbc2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "113ae6902d98261317b5507e55ac6e7758af81fc4660c34130490252640224a2" "d76af04d97252fafacedc7860f862f60d61fdcfbd026aeba90f8d07d8da51375" "01d8c9140c20e459dcc18addb6faebd7803f7d6c46d626c7966d3f18284c4502" "3328e7238e0f6d0a5e1793539dfe55c2685f24b6cdff099c9a0c185b71fbfff9" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "3f78849e36a0a457ad71c1bda01001e3e197fe1837cb6eaa829eb37f0a4bdad5" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (counsel-tramp counsel-projectile flx counsel ivy ivy-xref company-ghci flycheck-haskell ox-twbs unicode-fonts org-bullets nasm-mode cargo flycheck-rust racer rust-mode evil-escape ansible company-tern company-terraform js2-mode web-mode intero wolfram flycheck elm-mode flycheck-elm haskell-mode haskell-snippets sql-indent logstash-conf ix evil-ediff monky gnuplot-mode zenburn-theme ox-pandoc vagrant-tramp rainbow-delimiters json-mode evil-nerd-commenter sr-speedbar latex-preview-pane ansible-doc company-ansible jinja2-mode yasnippet company evil yaml-mode w3m ujelly-theme twilight-theme terraform-mode solarized-theme smex projectile paredit nlinum-relative multi-term markdown-mode+ magit flx-ido evil-surround evil-org erlang dash-at-point base16-theme auctex ag))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; You can now touch stuff


(load "~/.emacs.d/theme.el")
