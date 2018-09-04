(defvar my-idosmex-packages
  '(smex
    flx-ido))
(mapc #'package-install my-idosmex-packages)

(require 'flx-ido)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)

(setq-default
 ;; ido-use-faces nil
 ido-enable-flex-matching t
 ;; ido-max-window-height 1
 ido-create-new-buffer 'always)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'evil-leader)
(evil-leader/set-key
  "K" 'ido-kill-buffer
  "x" 'smex)

(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)) ; use ido for multiple snippets

(message "Loading init-smexido... Done.")
(provide 'init-smexido)
