(setq my-ansible-packages
      '(yaml-mode
        jinja2-mode
        company
        company-ansible
        ansible-doc))
(mapc #'package-install my-ansible-packages)

(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(message "Loading ansible-init...done")
(provide 'ansible-init)
