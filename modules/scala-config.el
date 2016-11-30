;;; package  --- scala-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : scala-config.el
;;; Description: Emacs configuration for Scala with Ensime
;;;              ENhanced Scala Interaction Mode for Emacs
;;;
;;; elisp code for customizing the scala development settings
;;;===========================================================================
(require 'ensime)             ; ensime loads scala-mode2 internally
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENhanced Scala Interaction Mode for Emacs (for scala development)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'scala-config)

;;; scala-config.el ends here
