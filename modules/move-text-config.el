;;; package  --- move-text-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : move-config.el
;;; Description: move text up or down in Emacs
;;; elisp code for customizing the text move settings
;;;
;;; Code:
;;;

;; Move current line or region with M-up or M-down
(require-package 'move-text)

;;
;; utility functions for moving region Up or Down use M-S-Up / M-S-Dn
;; utility functions for moving region Left or Right use C-S-L / C-S-R
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; keybd mapping for the above
;; (global-set-key [(meta shift up)]  'move-line-up)
;; (global-set-key [(meta shift down)]  'move-line-down)


;; for right or left movement of region
(defun shift-text (distance)
  "Shift the selected region right if DISTANCE is postive, left if negative."
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right ()
  "Shift right by a column."
  (interactive)
  (shift-text 1))

(defun shift-left ()
  "Shift left by a column."
  (interactive)
  (shift-text -1))

;; move the text in insert mode
(defun move-text-set-bindings ()
  "Use bindings for move-text-up and move-text-down (M-Shift-up / M-Shift-down)."
  (interactive)
  "Bind `move-text-up' and `move-text-down' to M-Shift-up & M-Shift-down."
  (global-set-key [(meta shift up)]  'move-text-up)
  (global-set-key [(meta shift down)]  'move-text-down)
  (global-set-key [C-S-right] 'shift-right)
  (global-set-key [C-S-left] 'shift-left))
(move-text-set-bindings)

(provide 'move-text-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; move-text-config.el ends here
