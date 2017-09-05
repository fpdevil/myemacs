;;; package  --- multiple-cursors-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : multiple-cursorss-config
;;; Description: Multiple cursors for editing in Emacs
;;;              http://www.star.bris.ac.uk/bjm/emacs-tips.html
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
(with-eval-after-load "multiple-cursors-autoloads"
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  ;; {{ multiple-cursors with evil
  ;; step 1, select thing in visual-mode (OPTIONAL)
  ;; step 2, `mc/mark-all-like-dwim' or `mc/mark-all-like-this-in-defun'
  ;; step 3, `ace-mc-add-multiple-cursors' to remove cursor, press RET to confirm
  ;; step 4, press s or S to start replace
  ;; step 5, press C-g to quit multiple-cursors
  (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
  (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
  (define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)
  ;; }}
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit multiple regions simultaneously in a buffer or a region           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)

;;---------------------------------------------------------------------------

(provide 'multiple-cursors-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; multiple-cursors-config.el ends here
