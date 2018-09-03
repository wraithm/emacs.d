(defvar my-rust-packages
  '(rust-mode
    racer
    cargo
    flycheck
    flycheck-rust))
(mapc #'package-install my-rust-packages)

(require 'racer)
(require 'rust-mode)
(require 'cargo)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)

(add-hook
 'rust-mode-hook
 (lambda ()
   (subword-mode)
   (racer-mode)
   (cargo-minor-mode)
   (flycheck-mode)
   (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(setq
 racer-cmd "~/.cargo/bin/racer"
 racer-rust-src-path "/Users/mwraith/src/Rust/rust/src")

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(message "Loading init-rust... Done.")
(provide 'init-rust)
