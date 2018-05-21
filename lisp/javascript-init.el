(require 'js2-mode)
(require 'flycheck)
(require 'company)
(require 'company-tern)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq-default flycheck-disabled-checkers
 (append flycheck-disabled-checkers
   '(javascript-jshint)))

(setq js2-include-node-externs t)

(flycheck-add-mode 'javascript-eslint 'js2-mode)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode)
            (company-mode)))

(message "Loading javascript-init...")
(provide 'javascript-init)