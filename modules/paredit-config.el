;;; package  --- paredit-config.el
;;;
;;; Commentary:
;;;            configuration file for paredit-module
;;;
;;; Filename: paredit-config.el
;;; Description: A minor mode for auto pairing the parenthesis
;;;
;;; elisp code for parentheses handling
;;===============================================================
(require 'paredit)
;;;
;;; Code:
;;;
(defun enable-paredit ()
  "Function for enabling the paredit globally."
  (paredit-mode 1))

(defvar lisp-mode-hooks
  '(clojure-mode-hook
    cider-repl-mode-hook
    ;;emacs-lisp-mode-hook
    ))

(dolist (h lisp-mode-hooks)
  (add-hook h 'enable-paredit))

(defun erlang-paredit ()
  "Paredit for Erlang major mode."
  (progn
    (define-key erlang-mode-map [?\(] 'paredit-open-parenthesis)
    (define-key erlang-mode-map [?\[] 'paredit-open-square)
    (define-key erlang-mode-map [?\{] 'paredit-open-curly)
    (define-key erlang-mode-map [?\)] 'paredit-close-parenthesis)
    (define-key erlang-mode-map [?\}] 'paredit-close-curly)
    (define-key erlang-mode-map [?\]] 'paredit-close-square)
    (set (make-local-variable 'paredit-space-for-delimiter-predicates)
     '((lambda (endp delimiter) nil)))))

(add-hook 'erlang-mode-hook
      (lambda ()
        (enable-paredit)
        (erlang-paredit)))

(provide 'paredit-config)

;;; paredit-config.el ends here
