;;; package  --- utils-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : utils-config.el
;;; Description: Some utilities and functions which does not have a proper
;;;              place or location
;;;              Using some functions from the excellent purcell Emacs.
;;; elisp code for customizing multiple things
;;;==========================================================================
(require 'switch-window) ;; visual replacement for (C-x o)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'winner-mode)
;; navigate the window layouts using the below
;; "C-c <left>"
;; "C-c <right>"

(setq-default switch-window-shortcut-style 'alphabet) ;; shortcut with alphabet
(setq-default switch-window-timeout nil)              ;; control cancel switching after timeout

;; Usage shortcut
(global-set-key (kbd "C-x o") 'switch-window)

;;---------------------------------------------------------------------------+
;; When splitting window, show (other-buffer) in the new window              |
;;---------------------------------------------------------------------------+
(defun split-window-func-with-other-buffer (split-function)
  "Take the SPLIT-FUNCTION and split a new window and push the open buffer into it."
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))

(defun aqua/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;;---------------------------------------------------------------------------+
;; rearrange the split windows as desired with the shortcuts                 |
;;---------------------------------------------------------------------------+
(defun split-window-horizontaly-instead ()
  "Split the window horizontally instead of randomly."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  "Split the window vertically instead of randomly."
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

;;---------------------------------------------------------------------------+
;; split window and toggle current window detection                          |
;;---------------------------------------------------------------------------+
(defun aqua/split-window ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'aqua/split-window)
      (progn
        (jump-to-register :aqua/unsplit-window))
    (window-configuration-to-register :aqua/split-window)
    (switch-to-buffer-other-window nil)))


(defun aqua/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactve)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "--> Window %s is dedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

;;---------------------------------------------------------------------------+
;; set the global keyboard shortcuts for window splitting                    |
;;---------------------------------------------------------------------------+
(global-set-key (kbd "C-x 1") 'aqua/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)
(global-set-key (kbd "<f7>") 'aqua/split-window)
(global-set-key (kbd "C-c <down>") 'aqua/toggle-current-window-dedication)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'utils-config)
;;; utils-config.el ends here
