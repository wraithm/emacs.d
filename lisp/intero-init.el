(defvar my-haskell-packages
      '(haskell-mode
        haskell-snippets
        intero))
(mapc #'package-install my-haskell-packages)


(require 'haskell)
(require 'haskell-mode)
(require 'haskell-snippets)
(require 'intero)

;; (load-file "~/.emacs.d/lisp/ghcid.el")
;; (require 'ghcid)

;; Minor-mode Hooks
(intero-global-mode 1)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'yas-minor-mode)
;; (add-hook 'haskell-mode-hook #'hindent-mode)
;; (add-hook 'haskell-mode-hook 'subword-mode)

(defun stack-compile-command ()
  (interactive)
  (setq-local compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options=-Werror"))

(add-hook 'haskell-mode-hook 'stack-compile-command)
(add-hook 'haskell-cabal-mode-hook 'stack-compile-command)

(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;; Variables
(setq
  haskell-hoogle-url "http://127.0.0.1:8123/?hoogle=%s"

  haskell-stylish-on-save t
  ;; haskell-tags-on-save t

  haskell-indentation-layout-offset 4
  haskell-indentation-left-offset 4

  ;; haskell-indentation-ifte-offset 4
  ;; haskell-indentation-show-indentations t
  ;; haskell-indentation-show-indentations-after-eol t

  )

;; Key bindings
(define-key haskell-mode-map (kbd "M-]") 'intero-goto-definition)
(define-key haskell-mode-map (kbd "C-c h") 'hoogle)
(evil-leader/set-key-for-mode 'haskell-mode "h" 'hoogle)

(require 'evil)
(evil-define-motion my-haskell-navigate-imports ()
  "Navigate imports with evil motion"
  :jump t
  :type line
  (haskell-navigate-imports))
(evil-leader/set-key-for-mode 'haskell-mode "i" 'my-haskell-navigate-imports)

(defun my-intero-insert-type ()
  "Insert type at point."
  (interactive)
  (intero-type-at t))
;; (evil-leader/set-key-for-mode 'haskell-mode "t" 'my-intero-insert-type)
(evil-leader/set-key-for-mode 'haskell-mode "t" 'intero-type-at)
(evil-define-key 'normal haskell-mode-map (kbd "C-c C-t") 'my-intero-insert-type)

(evil-set-initial-state 'intero-repl-mode 'emacs)

;; Alignment
(require 'align)
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

;; Evil indentation helper
(defun haskell-indentation-indent-line ()
  "Indent current line, cycle though indentation positions.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
  (interactive)
  ;; try to repeat
  (when (not (haskell-indentation-indent-line-repeat))
    (setq haskell-indentation-dyn-last-direction nil)
    ;; parse error is intentionally not caught here, it may come from
    ;; `haskell-indentation-find-indentations', but escapes the scope
    ;; and aborts the operation before any moving happens
    (let* ((cc (current-column))
           (ci (haskell-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (or (haskell-indentation-find-indentations)
                       '(0))))
           (valid (memq ci inds))
           (cursor-in-whitespace (< cc ci))
           ;; certain evil commands need the behaviour seen in
           ;; `haskell-indentation-newline-and-indent'
           (evil-special-command (and (bound-and-true-p evil-mode)
                                      (memq this-command '(evil-open-above
                                                           evil-open-below
                                                           evil-replace))))
           (on-last-indent (eq ci (car (last inds)))))
      (if (and valid cursor-in-whitespace)
          (move-to-column ci)
        (haskell-indentation-reindent-to
         (funcall
          (if on-last-indent
              #'haskell-indentation-previous-indentation
            #'haskell-indentation-next-indentation)
          (if evil-special-command
              (save-excursion
                (end-of-line 0)
                (1- (haskell-indentation-current-indentation)))
            ci)
          inds
          'nofail)
         cursor-in-whitespace))
      (setq haskell-indentation-dyn-last-direction (if on-last-indent 'left 'right)
            haskell-indentation-dyn-last-indentations inds))))


;; Haskell fast modules
(setq haskell-import-mapping
      '(("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
        ("Data.ByteString" . "import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Char8" . "import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
        ("Data.ByteString.Lazy.Char8" . "import qualified Data.ByteString.Lazy.Char8 as L8
")
        ("Data.Map" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
        ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
        ("Data.Set" . "import qualified Data.Set as S
import Data.Set (Set)
")
        ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")
        ("System.IO.Streams" . "import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
")
        ("Control.Monad" . "import Control.Monad ()
")
        ("Control.Monad.Trans.Class" . "import Control.Monad.Trans.Class (lift)
")
        ("Control.Monad.IO.Class" . "import Control.Monad.IO.Class (MonadIO, liftIO)
")
        ("Data.Monoid" . "import Data.Monoid ((<>))
")
        ("Data.Bool" . "import Data.Bool (bool)
")
        ("Data.Proxy" . "import Data.Proxy (Proxy (..))
")
        ))

(defvar haskell-fast-module-list
  (list)
  "A list of modules.")

(defun haskell-fast-modules-save ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/.emacs.d/.haskell-modules.el")
    (erase-buffer)
    (insert (format "%S" haskell-fast-module-list))
    (basic-save-buffer)
    (bury-buffer)))

(defun haskell-fast-modules-load ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/.emacs.d/.haskell-modules.el")
    (setq haskell-fast-module-list (read (buffer-string)))
    (bury-buffer)))

(defun haskell-capitalize-module (m)
  ;; FIXME:
  (with-temp-buffer
    (insert m)
    (upcase-initials-region (point-min) (point-max))
    (buffer-string)))

(defun haskell-fast-get-import (custom)
  (if custom
      (let* ((module (haskell-capitalize-module (read-from-minibuffer "Module: " ""))))
        (unless (member module haskell-fast-module-list)
          (add-to-list 'haskell-fast-module-list module)
          (haskell-fast-modules-save))
        module)
    (let ((module (haskell-capitalize-module
                   (haskell-complete-module-read
                    "Module: "
                    (append (mapcar #'car haskell-import-mapping)
                            haskell-fast-module-list)))))
      (unless (member module haskell-fast-module-list)
        (add-to-list 'haskell-fast-module-list module)
        (haskell-fast-modules-save))
      module)))

(defun haskell-fast-add-import (custom)
  "Add an import to the import list.  Sorts and aligns imports,
unless `haskell-stylish-on-save' is set, in which case we defer
to stylish-haskell."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (haskell-navigate-imports)
    (let* ((chosen (haskell-fast-get-import custom))
           (module (let ((mapping (assoc chosen haskell-import-mapping)))
                     (if mapping
                         (cdr mapping)
                       (concat "import " chosen "\n")))))
      (insert module))
    ;; (haskell-sort-imports)
    ;; (haskell-align-imports)
    ))

(evil-leader/set-key-for-mode 'haskell-mode "m" 'haskell-fast-add-import)

(haskell-fast-modules-load)
;; Haskell fast modules


;; w3m haddock
(require 'w3m)
(require 'w3m-haddock)
(setq haskell-w3m-haddock-dirs
      '("~/bitnomial/.stack-work/install/x86_64-osx/nightly-2017-09-06/8.2.1/doc"))

(setq w3m-mode-map (make-sparse-keymap))

(define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url)
(define-key w3m-mode-map (kbd "q") 'bury-buffer)
(define-key w3m-mode-map (kbd "<mouse-1>") 'w3m-maybe-url)
(define-key w3m-mode-map [f5] 'w3m-reload-this-page)
(define-key w3m-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
(define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
(define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
(define-key w3m-mode-map (kbd "M-.") 'w3m-haddock-find-tag)

(defun w3m-maybe-url ()
  (interactive)
  (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
          (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
      (w3m-view-this-url)))

(add-hook 'w3m-display-hook 'w3m-haddock-display)
;; w3m haddock


(message "Loading haskell-init...")
(provide 'intero-init)
