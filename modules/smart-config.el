;;; package  --- smart-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : smart-config.el
;;; Description: Smartparens is a minor mode for dealing with pairs in Emacs.
;;;
;;; elisp code for customizing the smartparens settings
;;;===========================================================================
(require 'smartparens-config)
;(require 'smartparens)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart parenthesis matching a minor mode for Emacs that deals with parens ;;
;; pairs and tries to be smart about it.                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(smartparens-global-mode t)
(show-smartparens-global-mode t)


(sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)


(provide 'smart-config)
;;; smart-config.el ends here
