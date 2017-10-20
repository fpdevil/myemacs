;;; package  --- psgml-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : psgml-config.el
;;; Description: Emacs major mode for editing Markup languages
;;; SGML covers XML, HTML, and lots of other markup languages
;;;==========================================================================

;;;
;;; Code:
;;;

(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(setq auto-mode-alist
       (nconc
         '(("\\.html$" . xml-mode))
         '(("\\.xml$" . xml-mode))
         auto-mode-alist))

;; syntax coloring
(add-hook 'xml-mode-hook   ; XML-specific settings
        (function (lambda()

; faces creation
(make-face 'sgml-comment-face)
(make-face 'sgml-start-tag-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-doctype-face)

; faces definitions
(set-face-foreground 'sgml-comment-face "SeaGreen")
(set-face-foreground 'sgml-start-tag-face "OrangeRed")
(set-face-foreground 'sgml-end-tag-face "OrangeRed")
(set-face-foreground 'sgml-doctype-face "MintCream")

; markup to face mappings
; (see http://www.lysator.liu.se/~lenst/about_psgml/psgml.html#Highlight for details)
(setq sgml-markup-faces
      '((comment . sgml-comment-face)
        (start-tag . sgml-start-tag-face)
        (end-tag . sgml-end-tag-face)
        (doctype . sgml-doctype-face)
        )
      )

; turn faces on
(setq sgml-set-face t)

)))



(provide 'psgml-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; psgml-config.el ends here
