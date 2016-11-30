;;; package --- customize python configuration for Emacs
;;;
;;; Commentary:
;;; Filename: popwin-config.el
;;; Description: popup window manager for Emacs
;;;              Treat some windows as popups and close them when they
;;;              are not used anymore
;;;===========================================================================
(require 'popwin)
;;;
;;; Code:
;;;
(popwin-mode 1)
(add-to-list 'popwin:special-display-config '"*jedi:doc*")

(provide 'popwin-config)

;;; popwin-config.el ends here
