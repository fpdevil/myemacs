;;; package --- which-func-config.el
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : which-func-config.el
;;; Description: Emacs settings for displaying function in scope
;;;
;;; elisp code for customizing the Emacs package which-function
;;;
;;; Code:
;;;
;;===============================================================================
; (require 'which-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show the name of the current function definition in the modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode 1)

;; Show the current function name in the header line
; (setq-default header-line-format
;               '((which-func-mode ("" which-func-format "--"))))

; (setq mode-line-misc-info
;       ;; We remove Which Function Mode from the mode line,
;       ;; because it's mostly invisible here anyway.
;       (assq-delete-all 'which-func-mode mode-line-misc-info))

(make-local-variable 'which-func-cleanup-function)
(setq which-func-cleanup-function #'string-trim-left)

;; to remove the ??? when which-func cannot determine name
;; (setq which-func-unknown "n/a")

;;;;;;;;;;;;;;;;;;;;;;; which func configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'which-func-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; which-func-config.el ends here