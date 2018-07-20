(require 'ix)
(require 'netrc)

(setq netrc-file "~/.netrc")

(defun netrc-username (machine)
  "Helper function to extract a username from my netrc."
  (car (netrc-credentials machine)))

(defun netrc-password (machine)
  "Helper function to extract a password from my netrc."
  (cadr (netrc-credentials machine)))

(setq ix-user (netrc-username "ix.io")
      ix-token (netrc-password "ix.io"))

(provide 'ix-init)
