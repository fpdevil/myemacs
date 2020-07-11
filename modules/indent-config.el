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
;;;
;;; Code:
;;;
;;;===========================================================================
(lazy-init
    (require-package 'aggressive-indent)
    (require 'aggressive-indent)

    ;; to enable globally for all modes
    ;; (global-aggressive-indent-mode 1)

    ;; enable for certain modes
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'inferior-emacs-lisp-mode #'aggressive-indent-mode)
    (add-hook 'ielm-mode #'aggressive-indent-mode)
    (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojurescript-mode-hook #'aggressive-indent-mode)

    ;; if a prticular mode needs to be disabled while enabling globally
    ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
    )

(provide 'indent-config)
;;; indent-config.el ends here
