;;; package --- dumbjump-config.el
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : dumbjump-config.el
;;; Description: Emacs settings for jumping to function definition
;;;
;;; elisp code for customizing the Emacs package dumbjump
;;;
;;; Code:
;;;
;;===============================================================================

;;------------------------------------------------------------------------------
;; jump to definition
;;------------------------------------------------------------------------------
(use-package dumb-jump
  :defer t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g l" . dumb-jump-quick-look)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'ivy)
  :ensure)

;;;;;;;;;;;;;;;;;;;;;;;; dumbjump configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dumbjump-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; dumbjump-config.el ends here
