;;; package  --- smartparens-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : smartparens-config.el
;;; Description: Smartparens is a minor mode for dealing with pairs in Emacs.
;;;
;;; elisp code for customizing the smartparens settings
;;;===========================================================================
(require 'smartparens)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart parenthesis matching a minor mode for Emacs that deals with parens ;;
;; pairs and tries to be smart about it.                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(smartparens-global-mode t)
(show-smartparens-global-mode t)


(provide 'smartparens-config)

;;; smartparens-config.el ends here
