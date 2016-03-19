;; ocaml-init
;; Matthew Wraith <wraithm@gmail.com>

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)
(require 'tuareg)
(require 'ocp-indent)

;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)

;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)

;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

(message "Loading ocaml-init... Done.")
(provide 'ocaml-init)
