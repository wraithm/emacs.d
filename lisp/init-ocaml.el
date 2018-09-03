;; ocaml-init
;; Matthew Wraith <wraithm@gmail.com>

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)
(require 'tuareg)
(require 'ocp-indent)

;; ocp-indent
(setq ocp-indent-config "strict_with=always,with=0,strict_comments=false")

;; Start merlin on ocaml files
(remove-hook 'tuareg-mode-hook "ocamlbuild")
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

;; Enable company auto-complete
(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))

;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;; Load utop
(autoload 'utop "utop" "Toplevel for OCaml" t)
(setq utop-command "opam config exec -- utop -emacs")
(evil-set-initial-state 'utop-mode 'emacs)
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode 'utop-minor-mode)

(message "Loading ocaml-init... Done.")
(provide 'ocaml-init)
