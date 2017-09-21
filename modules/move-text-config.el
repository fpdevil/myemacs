;;; package  --- move-text-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : move-config.el
;;; Description: move text up or down in Emacs
;;;
;;; elisp code for customizing the text move settings
;;;===========================================================================

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions for moving lines Up or Down                      ;;
;; use M-S-Up / M-S-Dn                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun move-text-get-region-and-prefix ()
    "Get the region and prefix for the `interactive' macro, without aborting.

Note: `region-beginning' and `region-end' are the reason why an
`interactive' macro with \"r\" will blow up with the error:

\"The mark is not set now, so there is no region\"

So the predicate `region-active-p' is needed to avoid calling
them when there's no region."
    `(,@(if (region-active-p)
            (list (region-beginning) (region-end))
          (list nil nil))
      ,current-prefix-arg))

(defun move-text--total-lines ()
  "Convenience function to get the total lines in the buffer / or narrowed buffer."
  (line-number-at-pos (point-max)))

(defun move-text--at-first-line-p ()
  "Predicate, is the point at the first line?"
  (= (line-number-at-pos) 1))

(defun move-text--at-penultimate-line-p ()
  "Predicate, is the point at the penultimate line?"
  (= (line-number-at-pos) (1- (move-text--total-lines))))

;; save-mark-and-excursion in Emacs 25 works like save-excursion did before
(eval-when-compile
  (when (< emacs-major-version 25)
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))

(defun move-text--last-line-is-just-newline ()
  "Predicate, is last line just a newline?"
  (save-mark-and-excursion
   (goto-char (point-max))
   (beginning-of-line)
   (= (point-max) (point))))

(defun move-text--at-last-line-p ()
  "Predicate, is the point at the last line?"
  (= (line-number-at-pos) (move-text--total-lines)))

(defun move-text-line-up ()
  "Move the current line up."
  (interactive)
    (if (move-text--at-last-line-p)
        (let ((target-point))
          (kill-whole-line)
          (forward-line -1)
          (beginning-of-line)
          (setq target-point (point))
          (yank)
          (unless (looking-at "\n")
            (newline))
          (goto-char target-point))
      (let ((col (current-column)))
        (progn (transpose-lines 1)
               (forward-line -2)
               (move-to-column col)))))

(defun move-text-line-down ()
  "Move the current line down."
  (interactive)
  (unless (or
           (move-text--at-last-line-p)
           (and
            (move-text--last-line-is-just-newline)
            (move-text--at-penultimate-line-p)))
    (let ((col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col))))

(defun move-text-region (start end n)
  "Move the current region (START END) up or down by N lines."
  (interactive (move-text-get-region-and-prefix))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-text-region-up (start end n)
  "Move the current region (START END) up by N lines."
  (interactive (move-text-get-region-and-prefix))
  (move-text-region start end (if (null n) -1 (- n))))

(defun move-text-region-down (start end n)
  "Move the current region (START END) down by N lines."
  (interactive (move-text-get-region-and-prefix))
  (move-text-region start end (if (null n) 1 n)))

(defun move-text-up (&optional start end n)
  "Move the line or region (START END) up by N lines."
  (interactive (move-text-get-region-and-prefix))
  (if (not (move-text--at-first-line-p))
    (if (region-active-p)
        (move-text-region-up start end n)
      (if n (cl-loop repeat n do (move-text-line-up))
        (move-text-line-up)))))

(defun move-text-down (&optional start end n)
  "Move the line or region (START END) down by N lines."
  (interactive (move-text-get-region-and-prefix))
  (if (region-active-p)
      (move-text-region-down start end n)
    (if n (cl-loop repeat n do (move-text-line-down))
      (move-text-line-down))))

(defun move-text-set-bindings ()
  "Use bindings for move-text-up and move-text-down (M-Shift-up / M-Shift-down)."
  (interactive)
  "Bind `move-text-up' and `move-text-down' to M-Shift-up & M-Shift-down."
  (global-set-key [(meta shift up)]  'move-text-up)
  (global-set-key [(meta shift down)]  'move-text-down))

(move-text-set-bindings)

(provide 'move-text-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; move-text-config.el ends here
