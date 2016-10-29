;;; package  --- flycheck-colors-config.el
;;;
;;; Commentary:
;;;
;;; Filename: flycheck-colors-config.el
;;; Description: configuration file for flycheck colors.
;;;              A minor mode for handling the errors in mode line
;;;
;;; elisp code for handling flycheck errors in colors
;;===============================================================

(require 'flycheck)

;;; Code:

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
     ;; indicate syntax errors/warnings in the left-fringe.
     (setq flycheck-indication-mode 'left-fringe)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     (set-face-background 'flycheck-error "#E5E8E8")
     (set-face-foreground 'flycheck-error "#00796B")
     (set-face-background 'flycheck-warning "#E5E8E8")
     (set-face-foreground 'flycheck-warning "#775500")
     (set-face-attribute 'flycheck-color-mode-line-error-face
			 '(:inherit flycheck-fringe-error
				    :foreground "red" :weight normal))))

;-------------------------------------------------------------------------------
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
;-------------------------------------------------------------------------------

;;; Improved Haskell support for Flycheck
(require 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'flycheck-colors-config)
;;; flycheck-colors-config.el ends here
