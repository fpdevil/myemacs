;;; package --- custom configuration for Markdown Mode
;;;
;;; Commentary:
;;; Filename   : markdown-config.el
;;; description: elisp code for customizing markdown mode for Emacs
;;;
;;;===========================================================================
(require 'markdown-mode)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing files in markdown mode                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
    (append
        (list '("\\.text"     . markdown-mode)
              '("\\.md"       . markdown-mode)
              '("\\.mdml$"    . markdown-mode)
              '("\\.markdown" . markdown-mode))
        auto-mode-alist))

(defun markdown-imenu-index ()
  "Provide an imenu handler for Markdown mode."
  (let* ((patterns '((nil "^#\\([# ]*[^#\n\r]+\\)" 1))))
    (save-excursion
      (imenu--generic-function patterns))))

(defcustom markdown-imenu-generic-expression nil
  "Generic declarations for markdown titles."
  :group 'markdown-config
  :type 'string
  :safe 'stringp)

(setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
       ("h1"   "^# \\(.*\\)$" 1)
       ("h2"   "^## \\(.*\\)$" 1)
       ("h3"   "^### \\(.*\\)$" 1)
       ("h4"   "^#### \\(.*\\)$" 1)
       ("h5"   "^##### \\(.*\\)$" 1)
       ("h6"   "^###### \\(.*\\)$" 1)
       ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))
 (add-hook 'markdown-mode-hook
           (lambda ()
             (setq imenu-generic-expression markdown-imenu-generic-expression)))

(defun markdown-mode-hook-setup ()
  "Add the markdown mode hook."
  (setq imenu-create-index-function 'markdown-imenu-index))
(add-hook 'markdown-mode-hook 'markdown-mode-hook-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'markdown-config)
;;; markdown-config.el ends here
