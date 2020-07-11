;;; package  --- flycheck-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename.  : flycheck-config.el
;;; Description: configuration file for flycheck errors and colors.
;;;              on-the-fly syntax checking extension for GNU Emacs
;;;              a minor mode for handling the errors in mode line
;;;
;;; syntax checking for GNU Emacs - http://www.flycheck.org/
;;;
;;; Code:
;;;
;;;

(require-package 'flycheck)                   ;; flycheck lib
(require-package 'flycheck-color-mode-line)   ;; flycheck color mode line
(require-package 'flycheck-tip)               ;; show flycheck/flymake errors indent tooltip
(require-package 'flycheck-pos-tip)           ;; flycheck errors display in tooltip

;;-----------------------------------------------------------------------------
;; custom function for enabling flycheck on demand
;;-----------------------------------------------------------------------------
(defvar syntax-checking-enable-by-default t
  "Enable syntax-checking by default.")

(defun add-flycheck-hook (mode)
  "Use flycheck in MODE by default, if `syntax-checking-enable-by-default' is true."
  (when (and syntax-checking-enable-by-default
             (listp flycheck-global-modes)
             (not (eq 'not (car flycheck-global-modes))))
    (push mode flycheck-global-modes)))

;;; == usage example ...
;; (add-flycheck-hook 'erlang-mode)

;;-----------------------------------------------------------------------------
;; additional flycheck options
;;-----------------------------------------------------------------------------

;; highlight the whole line, as it’s much faster
;; other options are nil, symbol etc
(setq flycheck-highlighting-mode 'lines)

;; indicate any syntax errors/warnings in the left-fringe.
(setq flycheck-indication-mode 'left-fringe)
(setq flycheck-highlighting-mode 'symbols)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; add on option for error messages
(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; Removing newline checks, as they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically
      '(save
        ;; new-line
        idle-change
        mode-enabled))

;; enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; helm based flycheck
(require-package 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(defun aqua/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 30.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'aqua/adjust-flycheck-automatic-syntax-eagerness)

;;-----------------------------------------------------------------------------
;; style flycheck errors consistently with flymake
;;-----------------------------------------------------------------------------
(eval-after-load 'flycheck
  '(progn
     ;; use an italic face for the checker name
     ;; (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic)
     (set-face-attribute 'flycheck-warning nil :foreground "OrangeRed2" :background "azure1")))

(custom-set-faces
 '(flycheck-error ((((class color)) (:underline "red"))))
 '(flycheck-info ((((class color)) (:underline "green"))))
 '(flycheck-warning ((((class color)) (:style wave :underline "orange")))))

;;-----------------------------------------------------------------------------
;; for flycheck-pos-tip
;;-----------------------------------------------------------------------------
(after 'flycheck
  (flycheck-pos-tip-mode))

;; flycheck inline messages
(require-package 'flycheck-inline)
(with-eval-after-load 'flycheck-inline
  (flycheck-inline-mode))

;;-----------------------------------------------------------------------------
;; toggle flycheck window
;;-----------------------------------------------------------------------------
(defun aqua/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun aqua/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

;;-----------------------------------------------------------------------------
;; flycheck error display
;;-----------------------------------------------------------------------------
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

;;-----------------------------------------------------------------------------
;; Show flycheck errors via posframe.el
;;-----------------------------------------------------------------------------
;;(use-package flycheck-posframe
;;  :defer t
;;  :after flycheck
;;  :config
;;  (setq flycheck-posframe-warning-prefix "⚠ "
;;        flycheck-posframe-info-prefix "··· "
;;        flycheck-posframe-error-prefix "✕ ")
;;  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))



(provide 'flycheck-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-config.el ends here
