(defvar my-haskell-packages
      '(haskell-mode
        haskell-snippets
        flycheck
        flycheck-haskell))
(mapc #'package-install my-haskell-packages)

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-snippets)
(require 'flycheck)
(require 'flycheck-haskell)
(require 'hindent)

(evil-set-initial-state 'haskell-interactive-mode 'emacs)

(add-hook
 'haskell-mode-hook
 (setq
  compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options=-Werror"
  hindent-style "johan-tibell"

  haskell-stylish-on-save t
  ;; haskell-tags-on-save t

  haskell-process-type 'stack-ghci

  haskell-indentation-layout-offset 4
  haskell-indentation-left-offset 4
  haskell-indentation-ifte-offset 4
  haskell-indentation-show-indentations t
  haskell-indentation-show-indentations-after-eol t))

(defun my-haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (flycheck-haskell-setup)
  (flycheck-select-checker 'haskell-ghc)
  (interactive-haskell-mode)
)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)


(evil-define-motion my-haskell-navigate-imports ()
  "Navigate imports with evil motion"
  :jump t
  :type line
  (haskell-navigate-imports))
(evil-leader/set-key-for-mode 'haskell-mode "i" 'my-haskell-navigate-imports)

(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-assignment
                  (regexp . "\\(\\s-+\\)=\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-arrows
                  (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-left-arrows
                  (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))

(message "Loading new-haskell-init... Done.")
(provide 'new-haskell-init)
