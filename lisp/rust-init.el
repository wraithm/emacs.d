(setq my-rust-packages
      '(rust-mode
        racer
        flycheck
        flycheck-rust))
(mapc #'package-install my-rust-packages)

(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)

(add-hook
 'rust-mode-hook
 (lambda ()
   (subword-mode)
   (company-mode)
   (racer-mode)
   (cargo-minor-mode)
   (flycheck-mode)
   (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))
 (setq
  racer-cmd "~/.cargo/bin/racer"
  racer-rust-src-path "/Users/mwraith/src/Rust/rust/src"))

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(message "Loading rust-init... Done.")
(provide 'rust-init)
