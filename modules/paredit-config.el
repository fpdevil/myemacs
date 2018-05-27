;;; package  --- paredit-config.el
;;;
;;; Commentary:
;;;            configuration file for paredit-module
;;;
;;; Filename: paredit-config.el
;;; Description: A minor mode for auto pairing the parenthesis
;;;
;;; elisp code for parentheses handling
;;;
;;; Code:
;;;
;;=============================================================================
(lazy-init
 (require-package 'paredit)
 (require 'paredit)


 (defun enable-paredit ()
   "Function for enabling the paredit globally."
   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
   (paredit-mode 1))

 (defvar lisp-mode-hooks
   '(clojure-mode-hook
     cider-repl-mode-hook
     ;;emacs-lisp-mode-hook
     ))

 (dolist (h lisp-mode-hooks)
   (add-hook h 'enable-paredit))

;;{{{ To use ParEdit with Emacs’ Lisp modes
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;}}}

 ;; (defun erlang-paredit ()
 ;;   "Paredit for Erlang major mode."
 ;;   (progn
 ;;     (define-key erlang-mode-map [?\(] 'paredit-open-parenthesis)
 ;;     (define-key erlang-mode-map [?\[] 'paredit-open-square)
 ;;     (define-key erlang-mode-map [?\{] 'paredit-open-curly)
 ;;     (define-key erlang-mode-map [?\)] 'paredit-close-parenthesis)
 ;;     (define-key erlang-mode-map [?\}] 'paredit-close-curly)
 ;;     (define-key erlang-mode-map [?\]] 'paredit-close-square)
 ;;     (set (make-local-variable 'paredit-space-for-delimiter-predicates)
 ;;      '((lambda (endp delimiter) nil)))))

 ;; (add-hook 'erlang-mode-hook
 ;;       (lambda ()
 ;;         (enable-paredit)
 ;;         (erlang-paredit)))
)
(provide 'paredit-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; paredit-config.el ends here
