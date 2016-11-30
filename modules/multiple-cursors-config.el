;;; package  --- multiple-cursors-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : multiple-cursorss-config
;;; Description: Multiple cursors for editing in Emacs
;;;
;;;
;;; elisp code for customizing the multiple-cursors package for Emacs
;;;===========================================================================
(require 'multiple-cursors)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors for Emacs                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(provide 'multiple-cursors-config)

;;; multiple-cursors-config.el ends here
