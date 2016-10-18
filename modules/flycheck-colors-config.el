;;===============================================================
;;; configuration file for flycheck colors
;; Filename: flycheck-colors-config.el
;; Description: A minor mode for handling the errors in mode line
;;
;;; Commentary:
;;
;; elisp code for handling flycheck errors in colors
;;===============================================================


(require 'flycheck)

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
     (set-face-background 'flycheck-error "red")
     (set-face-foreground 'flycheck-error "black")
     (set-face-background 'flycheck-warning "orange")
     (set-face-foreground 'flycheck-warning "black")))
