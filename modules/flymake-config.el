;;; package  --- flymake-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename.  : flymake-config.el
;;; Description: configuration file for flymake error checker.
;;;              FlyMake performs on-the-fly syntax checks on the files being
;;;              edited using the external syntax check tool (usually the compiler).
;;;
;;; syntax checking for GNU Emacs - https://www.emacswiki.org/emacs/FlyMake
;;; Code:
;;;===========================================================================
(require 'flymake)
(after 'flymake '(require 'flymake-cursor))

;; show errors and warnings as colored underlined text
(custom-set-faces
 '(flymake-errline ((((class color)) (:style wave :underline "Red"))))
 '(flymake-warnline ((((class color)) (:style wave :underline "Yellow")))))

;; show error and warning messages in mini buffer
 (defun my-flymake-show-help ()
   (when (get-char-property (point) 'flymake-overlay)
     (let ((help (get-char-property (point) 'help-echo)))
       (if help (message "%s" help)))))

 (add-hook 'post-command-hook 'my-flymake-show-help)

;; disable flymake to html file.
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flymake-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flymake-config.el ends here
