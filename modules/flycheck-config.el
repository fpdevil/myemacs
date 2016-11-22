;;; package  --- flycheck-config.el
;;;
;;; Commentary:
;;;
;;; Filename.  : flycheck-config.el
;;; Description: configuration file for flycheck errors and colors.
;;;              on-the-fly syntax checking extension for GNU Emacs
;;;              a minor mode for handling the errors in mode line
;;;
;;; syntax checking for GNU Emacs - http://www.flycheck.org/
;;;==============================================================

(require 'flycheck)                   ;; flycheck lib
(require 'helm-flycheck)              ;; flycheck helm integration
(require 'flycheck-color-mode-line)   ;; flycheck color mode line
(require 'flycheck-tip)               ;; show flycheck/flymake errors indent tooltip
(require 'flycheck-pos-tip)           ;; flycheck errors display in tooltip
(require 'flycheck-haskell)           ;; Improved Haskell support for Flycheck

;;; Code:

;;
; setting up customized flycheck
;;
(add-hook 'find-file-hook
          (lambda ()
            (when (not (equal 'emacs-lisp-mode major-mode))
              (flycheck-mode))))
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(progn
     ;(setq flycheck-highlighting-mode nil)
     (setq flycheck-highlighting-mode 'symbols)
     ;; enable flycheck globally
     (add-hook 'after-init-hook 'global-flycheck-mode)
     ;; helm based flycheck
     (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
     ;; indicate syntax errors/warnings in the left-fringe.
     (setq flycheck-indication-mode 'left-fringe)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     ;; for a cusomized display of error message
     (set-face-background 'flycheck-error "#E5E8E8")
     (set-face-foreground 'flycheck-error "#00796B")
     (set-face-background 'flycheck-warning "#E5E8E8")
     (set-face-foreground 'flycheck-warning "#775500")
     (set-face-attribute 'flycheck-color-mode-line-error-face
			 '(:inherit flycheck-fringe-error
				    :foreground "red" :weight normal))
     (setq flycheck-highlighting-mode 'symbols)
     (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
     (setq flycheck-check-syntax-automatically
       '(mode-enabled save new-line idle-change))))


;;------------------------------------------------------------------------------
; (require 'flycheck)
; (add-hook 'find-file-hook
;           (lambda ()
;             (when (not (equal 'emacs-lisp-mode major-mode))
;               (flycheck-mode))))

; (global-set-key (kbd "M-n") 'next-error)
; (global-set-key (kbd "M-p") 'previous-error)

; (global-set-key
;  (kbd "C-c c")
;  (lambda () (interactive)
;    (if flycheck-checker
;        (progn
;          (save-buffer)
;          (flycheck-compile flycheck-checker)))
;    (message
;     "No checker selected for this buffer. Try M-x flycheck-select-checker")))

; (require 'flycheck-color-mode-line)
; (require 'flycheck-pos-tip)

; (eval-after-load "flycheck"
;   '(progn
;      (setq flycheck-highlighting-mode 'symbols)
;      (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
;      (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;      (set-face-background 'flycheck-error "#660000")
;      (set-face-foreground 'flycheck-error nil)
;      (set-face-background 'flycheck-warning "#775500")
;      (set-face-foreground 'flycheck-warning nil)
;      (require 'flycheck-color-mode-line)
;      (set-face-background 'flycheck-color-mode-line-error-face "#660000")
;      (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
;      (set-face-background 'flycheck-color-mode-line-info-face nil)
;      (set-face-foreground 'flycheck-color-mode-line-error-face nil)
;      (set-face-foreground 'flycheck-color-mode-line-warning-face nil)
;      (set-face-foreground 'flycheck-color-mode-line-info-face nil)))
;;------------------------------------------------------------------------------

; for pos-tip
(with-eval-after-load 'flycheck
  '(flycheck-pos-tip-mode))
(setq flycheck-pos-tip-mode t)
; for haskell
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'flycheck-config)

;;; flycheck-config.el ends here
