;;; package  --- indent-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : indent-config.el
;;; Description: Emacs configuration for text indentation
;;;              Using aggressive-indent-mode which is an Emacs minor mode
;;;              that keeps your code always indented.
;;;
;;; elisp code for customizing ycm
;;; https://github.com/Malabarba/aggressive-indent-mode
;;;===========================================================================
(require 'aggressive-indent)
;;;
;;; Code
;;;
;; (global-aggressive-indent-mode 1) ;; enables globally

;; enable for certain modes
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'python-mode-hook #'aggressive-indent-mode)
(add-hook 'erlang-mode-hook #'aggressive-indent-mode)
(add-hook 'c++-mode-hook #'aggressive-indent-mode)

;; if a prticular mode needs to be disabled while enabling globally
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(provide 'indent-config)

;; indent-config.el ends here