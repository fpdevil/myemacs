;;; package  --- flymake-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename.  : flymake-config.el
;;; Description: configuration file for flymake error checker.
;;;              FlyMake performs on-the-fly syntax checks on the files being
;;;              edited using the external syntax check tool (usually the compiler).
;;;
;;; syntax checking for GNU Emacs - https://www.emacswiki.org/emacs/FlyMake
;;;
;;; Code:
;;;

(require 'flymake)
(after 'flymake '(require 'flymake-cursor)

       ;; set flymake log level
       (setq flymake-log-level 3)

       ;; show errors and warnings as colored underlined text
       (custom-set-faces
        '(flymake-errline ((((class color)) (:style wave :underline "red"))))
        '(flymake-warnline ((((class color)) (:style wave :underline "yellow")))))

       ;; show error and warning messages in mini buffer
       (defun my-flymake-show-help ()
         (when (get-char-property (point) 'flymake-overlay)
           (let ((help (get-char-property (point) 'help-echo)))
             (if help (message "%s" help)))))

       (add-hook 'post-command-hook 'my-flymake-show-help)

       ;; disable flymake to html file.
       (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
       (delete '("\\.tex?\\'" flymake-simple-tex-init) flymake-allowed-file-name-masks)

       (defun flymake-errors-on-current-line ()
         "Return the errors on the current line or nil if none exist"
         (let* ((line-no (flymake-current-line-no)))
           (nth 0 (flymake-find-err-info flymake-err-info line-no))))

       (defun flymake-display-err-message-for-current-line ()
         "Display a message with errors/warnings for current line if
it has errors and/or warnings."
         (interactive)
         (let* ((line-no             (flymake-current-line-no))
                (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
                (message-data        (flymake-make-err-menu-data line-no line-err-info-list)))
           (if message-data (progn (princ (car message-data) t)
                                   (mapcar (lambda (m)
                                             (terpri t)
                                             (princ (caar m) t))
                                           (cdr message-data)))
             (flymake-log 2 "no errors for line %d" line-no))))

       (defun flymake-mode-on-without-check ()
         "Turn flymake-mode on without the initial check"
         (let ((flymake-start-syntax-check-on-find-file nil))
           (flymake-mode-on)))

       (defun flymake-load-and-check-if-not-loaded (trigger-type)
         "If flymake is not loaded, load and start a check and return t. Otherwise return nil."
         (if flymake-mode
             nil
           (flymake-mode-on-without-check)
           (flymake-start-syntax-check trigger-type)
           t))

       (defun show-next-flymake-error ()
         "Load flymake.el if necessary. Jump to next error and display it."
         (interactive)
         (when (not (flymake-load-and-check-if-not-loaded "edit"))
           ;; if the cursor is on an error line and the user didn't just
           ;; cycle through error lines, just show the error of the current
           ;; line and don't skip to the next one
           (when (or (member last-command '(show-next-flymake-error show-prev-flymake-error))
                     (not (flymake-errors-on-current-line)))
             (flymake-goto-next-error))
           (flymake-display-err-message-for-current-line)))

       (defun show-prev-flymake-error ()
         "Jump to the previous flymake error and display it"
         (interactive)
         (when (not (flymake-load-and-check-if-not-loaded "edit"))
           (flymake-goto-prev-error)
           (flymake-display-err-message-for-current-line)))

       (global-set-key [f5] 'show-next-flymake-error)
       (global-set-key [S-f5] 'show-prev-flymake-error)

       (defun load-flymake-and-force-syntax-check ()
         "Load flymake.el if it was not loaded and start a check"
         (interactive)
         (flymake-mode-on-without-check)
         (flymake-start-syntax-check "force"))

       (defun enable-flymake-check-on-edit ()
         "Re-enable check-on-edit after a save or forced check disabled it"
         (interactive)
         (setq flymake-no-changes-timeout 0.5)
         (flymake-start-syntax-check "edit"))

       (setq flymake-extension-use-showtip t)  ;use `showtip' display error or warning.
       ;;(add-hook 'find-file-hook 'flymake-find-file-hook)
       )



(provide 'flymake-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flymake-config.el ends here
