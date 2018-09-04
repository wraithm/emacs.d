;; Font
(set-frame-font "Inconsolata-12")
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))

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

;; Window settings
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(left . 500))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defun open-frame-maximized ()
  (interactive)
  (modify-frame-parameters (make-frame) '((fullscreen . maximized))))

(defun open-frame-other-window ()
  (interactive)
  (modify-frame-parameters
   (make-frame)
   '((width . 250)
     (height . 100)
     (top . 250)
     (left + -1000)
     (fullscreen . maximized))))

;; Transparent titlebar
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Transparent background on terminal
;; TODO linums
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
    (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

;; Theme
(require 'solarized)
(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil)

;; Experimental, trying these out
(setq solarized-height-plus-1 1.0
      solarized-height-plus-2 1.0
      solarized-height-plus-3 1.0
      solarized-height-plus-4 1.0
      solarized-high-contrast-mode-line t)

(load-theme 'solarized-dark)
;; (load-theme 'solarized-light)

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
