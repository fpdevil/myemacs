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
(when (eq dotemacs-clr-identifiers 'rainbow-identifiers)

  ;; Highlight identifiers as per their names
  (require 'rainbow-identifiers)

  ;;------------------------------------------------------------------------------
  ;; fancy (but useful) stuff  for rainbow identifiers
  ;; customization's - use a wider set of colors
  ;;------------------------------------------------------------------------------
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
        rainbow-identifiers-cie-l*a*b*-saturation 100
        rainbow-identifiers-cie-l*a*b*-lightness 40
        ;; override theme faces
        ;; filter: don't mark identifiers inside comments or strings
        rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                                font-lock-keyword-face
                                                font-lock-function-name-face
                                                font-lock-variable-name-face))


  ;; add rainbow identifiers for most programming language modes
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

  )

;;------------------------------------------------------------------------------
(provide 'rbow-identifiers-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; rbow-identifiers-config.el ends here
