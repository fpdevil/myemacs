;;; package --- configuration settings for Emacs ipython notebook
;;;
;;; Commentary:
;;;
;;; Filename   : ipythonb-config.el
;;; Description: Emacs ipython notebook
;;;
;;; elisp code for customizing python code
;;;
;;; Code:
;;==============================================================================
(lazy-init
    (require-package 'ein)                        ;; Emacs iPython Notebook
    (require 'ein)
    (require 'ein-connect)
    (require 'ein-loaddefs)
    (require 'ein-notebook)
    (require 'ein-subpackages)

 (after 'auto-complete
   (setq ein:use-auto-complete-superpack t))

 (after 'jedi
   (setq ein:completion-backend 'ein:use-ac-jedi-backend)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ipythonnb-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ipythonnb-config.el ends here
