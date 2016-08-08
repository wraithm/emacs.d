;; Org
(require 'evil-org)

;; variables
(setq org-directory "~/Dropbox/org")
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location (concat (org-file-path "archive.org") "::* From %s"))
(setq org-agenda-files (list org-index-file))
(setq org-log-done 'time)

;; hooks
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(add-hook
 'org-mode-hook
 (lambda ()
   ;; (face-remap-add-relative 'default :family "Terminus (TTF)-12")
   (face-remap-add-relative 'default :family "Inconsolata-14")
   (turn-on-auto-fill)))
(setq org-todo-keywords
      '((sequence "TODO" "WORK" "DONE")))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(setq org-capture-templates
      '(("t" "Todo"
         entry
         (file org-index-file)
         "* TODO %?\n")

        ("b" "Blog idea"
         entry
         (file (org-file-path "blog-ideas.org"))
         "* TODO %?\n")))

;; Key bindings
(define-key global-map (kbd "C-c C-x C-s") 'mark-done-and-archive)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c f") 'org-agenda)
(define-key global-map (kbd "C-c l") 'org-store-link)

(message "Loading org-init... Done.")
(provide 'org-init)