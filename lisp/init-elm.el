;; init-elm
;; Matthew Wraith <wraithm@gmail.com>

;; Install packages
(defvar my-elm-packages
      '(elm-mode
        flycheck-elm
        ))
(mapc #'package-install my-elm-packages)

(require 'elm-mode)
(require 'flycheck)
(require 'flycheck-elm)

(with-eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(require 'company)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))

(add-hook 'elm-mode-hook
          (lambda ()
            (setq company-backends '(company-elm))))

(message "Loading init-elm... Done.")
(provide 'init-elm)
