;;; package  --- quick-peek-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : quick-config.el
;;; Description: Quick-peek inline-window library for Emacs
;;;
;;; elisp code for macro definition and custom settings
;;;===========================================================================
(require 'quick-peek)

;;;
;;; Code:
;;;

(set-face-attribute 'quick-peek-border-face nil :height 20)

(defmacro quick-peek-make-doc-command (doc-fun sym-fun)
  "Return an command that uses `quick-peek' to preview docs.

DOC-FUN is a unary function that takes a loop-up string and
returns the doc string.

SYM-FUN is a nullary function that gets the symbol at point as a
string."
  `(lambda ()
     (interactive)
     (let ((doc (funcall ,doc-fun (funcall ,sym-fun))))
       (if (string-empty-p doc)
           (message "Unknown symbol, or no documentation available.")
         (let ((map (make-sparse-keymap)))
           (dolist (scroll-key (where-is-internal #'mwheel-scroll))
             (define-key map scroll-key #'mwheel-scroll))
           (define-key map [t] #'quick-peek-hide)
           (set-transient-map map t))
         (let ((pos (save-excursion
                      (beginning-of-line)
                      (point))))
           (quick-peek-show doc pos nil (frame-height)))))))

(provide 'quick-peek-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; quick-peek-config.el ends here
