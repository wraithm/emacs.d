(defvar my-ivy-packages
  '(ivy
    counsel
    flx
    smex
    ivy-xref))
(mapc #'package-install my-ivy-packages)

;; Maybe add ivy-yasnippet?

(require 'ivy)
(require 'smex)
(ivy-mode t)

(require 'diminish)
(diminish 'ivy-mode)

(setq
 ivy-use-virtual-buffers t
 enable-recursive-minibuffers t
 ivy-virtual-abbreviate 'fullpath
 ivy-extra-directories nil ; no dired on double-tab or enter
 ivy-count-format "(%d/%d) ")

(require 'ivy-xref)
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)

(evil-leader/set-key
  "g" 'counsel-ag
  "G" 'ag
  "x" 'counsel-M-x)

(require 'projectile)
(setq projectile-completion-system 'ivy)

;; Emulate ido
(require 'flx)
(setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)

(require 'imenu)

(defun ivy-imenu-get-candidates-from (alist  &optional prefix)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                       (ivy-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        (concat prefix (if prefix ".") (car elm)))
                   (and (cdr elm) ; bug in imenu, should not be needed.
                        (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                        (list (cons (concat prefix (if prefix ".") (car elm))
                                    (copy-marker (cdr elm))))))))

(defun ivy-imenu-goto ()
  "Go to buffer position"
  (interactive)
  (let ((imenu-auto-rescan t) items)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (setq items (imenu--make-index-alist t))
    (ivy-read "imenu items:"
              (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
              :action (lambda (k) (goto-char k)))))


(provide 'ivy-init)
