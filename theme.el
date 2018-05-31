;; Font
(set-frame-font "Inconsolata-12")
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(width . 450))
(add-to-list 'default-frame-alist '(height . 150))

;; (set-frame-font "Source Code Pro-14")
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 140
;;                     :weight 'light
;;                     :width 'normal)

;; (setq mac-allow-anti-aliasing nil)
;; (set-frame-font "Terminus (TTF)-14")
;; (set-face-attribute 'default nil :family "Terminus (TTF)" :height 160)


;; Theme
(setq
 solarized-use-variable-pitch nil
 solarized-scale-org-headlines nil)
;; (load-theme 'solarized-light)
(load-theme 'solarized-dark)

;; (load-theme 'zenburn)

;; (load-theme 'wombat t)

;; (if window-system
;;     (load-theme 'base16-tomorrow-dark))

;; (require 'moe-theme)
;; (setq moe-theme-highlight-buffer-id t) ; moe-theme settings
;; (setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
;; (setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
;; (setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
;; ;; (moe-dark)
;; (moe-light)
