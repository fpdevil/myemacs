;;; package  --- iedit-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : iedit-config
;;; Description: Edit multiple regions simultaneously in a buffer or a region
;;;              http://www.emacswiki.org/Iedit
;;;
;;; elisp code for customizing the iedit package settings
;;;
;;; Code:
;;;
;;;=============================================================================
(require 'iedit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit multiple regions simultaneously in a buffer or a region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(after 'iedit
  (setq iedit-unmatched-lines-invisible-default t)
  ;; fix a bug for iedit in Mac
  (define-key global-map (kbd "C-c ;") 'iedit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'iedit-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; iedit-config.el ends here
