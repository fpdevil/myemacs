;;; package --- rbow-identifiers.config.el configuration settings
;;;
;;; Commentary:
;;;
;;; Filename   : rbow-identifiers-config.el
;;; Description: rainbow identifiers is a minor mode for Emacs which displays the
;;;              variables in multiple colors.
;;;
;;; Code:
;;;

(when (eq dotemacs-clr-identifiers 'rainbow-identifiers)

  ;; Highlight identifiers as per their names
  (require 'rainbow-identifiers)

  ;; add rainbow identifiers for most programming language modes
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

  (defun rainbow-identifiers-filter (beg end)
    "Only highlight standalone words or those following 'this.' or 'self.'"
    (let ((curr-char (char-after beg))
          (prev-char (char-before beg))
          (prev-self (buffer-substring-no-properties
                      (max (point-min) (- beg 5)) beg)))
      (and (not (member curr-char
                        '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ??)))
           (or (not (equal prev-char ?\.))
               (equal prev-self "self.")
               (equal prev-self "this.")))))

  (setq rainbow-identifiers-faces-to-override
        '(font-lock-type-face
          font-lock-keyword-face
          font-lock-variable-name-face
          font-lock-function-name-face))

  ;; set the filter
  (add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)
  )

(provide 'rbow-identifiers-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; rbow-identifiers-config.el ends here
