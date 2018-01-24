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
;;;=============================================================================
(when (eq dotemacs-clr-identifiers 'rbow-identifiers)

 ;; Highlight identifiers as per their names
 (require 'rainbow-identifiers)

 ;;------------------------------------------------------------------------------
 ;; fancy (but useful) stuff  for rainbow identifiers
 ;; customization's - use a wider set of colors
 ;;------------------------------------------------------------------------------
 (setq rainbow-identifiers-choose-face-function
       'rainbow-identifiers-cie-l*a*b*-choose-face)
 (setq rainbow-identifiers-cie-l*a*b*-lightness 100)
 (setq rainbow-identifiers-cie-l*a*b*-saturation 40)

 ;; rainbow identifier customization's - customized filter: don't mark *all* identifiers
 (defun rainbow-identifiers-filter (beg end)
   "BEG END Only highlight standalone words or those following 'this.' or 'self.'."
   (let ((curr-char (char-after beg))
         (prev-char (char-before beg))
         (prev-self (buffer-substring-no-properties
                     (max (point-min) (- beg 5)) beg)))
     (and (not (member curr-char
                       '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ??)))
          (or (not (equal prev-char ?\.))
              (equal prev-self "self.")
              (equal prev-self "this.")))))

 ;; filter: don't mark identifiers inside comments or strings
 (setq rainbow-identifiers-faces-to-override
       '(font-lock-keyword-face
         font-lock-variable-name-face
         font-lock-function-name-face
         highlight-quoted-symbol))

 ;; set a filter for the identifiers
 (add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)

 ;; add rainbow identifiers for most programming language modes
 ;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
 )

;;------------------------------------------------------------------------------
(provide 'rbow-identifiers-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; rbow-identifiers-config.el ends here
