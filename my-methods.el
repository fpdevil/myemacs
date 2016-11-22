;;; package --- my-methods.el
;;;
;;; Commentary:
;;; Filename   : my-methods.el
;;; Description: any custom methods are defined here
;;;
;;; Code:
;;; Updated    : 14 Nov 2016
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-command-buffer-hooks (make-hash-table))

(defun my-command-on-save-buffer (c)
    "C Run a command <c> every time the buffer is saved."
    (interactive "sShell command: ")
    (puthash (buffer-file-name) c my-command-buffer-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-command-buffer-kill-hook ()
  "Remove a key from <command-buffer-hooks> if it exists."
  (remhash (buffer-file-name) my-command-buffer-hooks))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-command-buffer-run-hook ()
  "Run a command if it exists in the hook."
  (let ((hook (gethash (buffer-file-name) my-command-buffer-hooks)))
    (when hook
        (shell-command hook))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-recompile-init-files ()
  "Recompile all of the startup files."
  (interactive)
  (byte-recompile-directory (concat (getenv "HOME") "/.emacs.d/packages") 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add inserting current date time.
(defun my-insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
     two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d %H:%M")
                   ((equal prefix '(4)) "%Y-%m-%d")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          )
      (insert (format-time-string format))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get current system's name
(defun insert-system-name()
(interactive)
"Get current system's name"
(insert (format "%s" system-name))
)

;; Get current system type
(defun insert-system-type()
(interactive)
"Get current system type"
(insert (format "%s" system-type))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; add hooks
;;
(add-hook 'kill-buffer-hook 'my-command-buffer-kill-hook)
(add-hook 'after-save-hook 'my-command-buffer-run-hook)

(provide 'my-methods)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; my-methods.el ends here
