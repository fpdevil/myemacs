;;; package --- miscellaneous python helper configuration settings
;;;
;;; Commentary:
;;;
;;; Filename   : python-helper-config.el
;;; Description: Miscellaneous Python helper configurations
;;;
;;; elisp code for customizing python code
;;===========================================================================

;;;
;;; Code:
;;;

(defcustom python-autopep8-path (executable-find "autopep8")
  "autopep8 executable path."
  :group 'python
  :type 'string)

(defun python-autopep8 ()
  "Automatically formats Python code to conform to the PEP 8 style guide.
$ autopep8 --in-place --aggressive --aggressive <filename>"
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command
     (format "%s --in-place --aggressive %s" python-autopep8-path
             (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

;; for auto format on save
(eval-after-load 'python
  '(if python-autopep8-path
       (add-hook 'before-save-hook 'python-autopep8)))

(provide 'python-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; python-helper-config.el ends here
