;;; package  --- sp-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : sp-config.el
;;; Description: Smartparens is a minor mode for dealing with pairs in Emacs.
;;;
;;; elisp code for customizing the smartparens settings
;;;
;;; Code:
;;;
;;;===========================================================================

(lazy-init

 (require 'smartparens-config)

 (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
 (setq sp-autoinsert-pair t)

 (setq sp-show-pair-delay 0)
 (setq sp-show-pair-from-inside t)

 ;;** smart parenthesis matching a minor mode for Emacs that deals with parens
 ;;** pairs and tries to be smart about it.
 (show-smartparens-global-mode t)
 (smartparens-global-mode t)

 ;;** integration with paredit
 ;; (sp-use-smartparens-bindings)
 (sp-use-paredit-bindings)

 ;;** sp modes
 (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
 (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
 ;; set up some pairings for org mode markup. These pairings won't
 ;; activate by default; they'll only apply for wrapping regions.
 (sp-local-pair 'org-mode "~" "~" :actions '(wrap))
 (sp-local-pair 'org-mode "/" "/" :actions '(wrap))
 (sp-local-pair 'org-mode "*" "*" :actions '(wrap))

 (sp-with-modes '(tex-mode
                  plain-tex-mode
                  latex-mode
                  org-mode)
   (sp-local-pair "$" " $")
   (sp-local-pair "\\[" " \\]")
   (sp-local-pair "\\(" " \\)")
   (sp-local-pair "\\{" " \\}")
   (sp-local-pair "\\left(" " \\right)")
   (sp-local-pair "\\left\\{" " \\right\\}"))

 (sp-with-modes '(emacs-lisp-mode
                  inferior-emacs-lisp-mode
                  lisp-interaction-mode
                  lisp-mode)
   (sp-local-pair "'" nil :actions nil)
   (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

 (sp-with-modes '(scala-mode)
   (sp-local-pair "'" nil :actions nil))

 (sp-with-modes '(text-mode)
   (sp-local-pair "`" "'" :actions '(insert wrap)))

 (sp-with-modes '(markdown-mode
                  rst-mode)
   (sp-local-pair "`" "`"))

 (setq sp-base-key-bindings 'paredit)
 (setq sp-autoskip-closing-pair 'always)
 (setq sp-hybrid-kill-entire-symbol nil)
 (setq blink-matching-paren t)
 (setq sp-navigate-close-if-unbalanced t)

 (custom-set-faces
  '(sp-pair-overlay-face ((t (:background "grey20")))))

 ;; angle brackets aren’t treated as delimiters by default in js2-mode
 ;; below is the fix through sp
 (eval-after-load 'js2-mode
   '(sp-local-pair 'js2-mode "<" ">"))
 (eval-after-load 'html-mode
   '(sp-local-pair 'html-mode "<" ">"))

 ;;** newline and indentation after {}
(sp-local-pair 'js-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'javascript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


 ;;** for c/c++ indentation
 (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

 (defun my-create-newline-and-enter-sexp (&rest _ignored)
   "Open a new brace or bracket expression, with relevant newlines and indent."
   (newline)
   (indent-according-to-mode)
   (forward-line -1)
   (indent-according-to-mode))

 ;;** fix an error caused by smartparens interfering with yasnippet' bindings
 (after 'yasnippet
   (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

 )

(provide 'sp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; sp-config.el ends here
