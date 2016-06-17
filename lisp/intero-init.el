(setq my-haskell-packages
      '(haskell-mode
        haskell-snippets

        flycheck
        intero))
(mapc #'package-install my-haskell-packages)

(require 'haskell)
(require 'haskell-mode)
(require 'haskell-snippets)

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec ag -i --numbers %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

;; Sub-mode Hooks
(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'subword-mode)

;; Variables
(add-hook
 'haskell-mode-hook
 (setq
  haskell-indentation-layout-offset 4
  haskell-indentation-left-offset 4
  haskell-indentation-ifte-offset 4
  haskell-indentation-show-indentations t
  haskell-indentation-show-indentations-after-eol t
  haskell-stylish-on-save t))

;; Key bindings
(global-set-key (kbd "M-g M-f") 'first-error)

(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)

(evil-set-initial-state 'intero-REPL-mode 'emacs)

;; Alignment
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

(global-set-key (kbd "C-x a r") 'align-regexp)


(message "Loading haskell-init... Done.")
(provide 'intero-init)
