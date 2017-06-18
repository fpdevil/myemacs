;;; package  --- ac-complete-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : ac-complete-config.el
;;; Description: Modular in-buffer completion framework for Emacs
;;;              Auto-Complete is a text completion framework for Emacs.
;;;
;;;
;;; elisp code for standard auto-completion configuration with auto-complete
;;;===========================================================================
(require 'auto-complete)
(require 'pos-tip)
(require 'ac-slime)

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-completion (where company is not available)                         ;;
;; setting up autocomplete after yasnippet to avoid duplicate tab bindings  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories
               (concat user-emacs-directory "/cache/auto-complete/ac-dict"))
  (setq ac-comphist-file
        (concat user-emacs-directory "/cache/auto-complete/ac-comphist.dat"))
  (ac-config-default))

;;===========================================================================
;; some default global values for ac completions
;;===========================================================================
(setq ac-auto-show-menu t
      ac-delay 0.6
      ac-quick-help-delay 0.8
      ac-auto-start 2
      ac-use-menu-map t
      ac-menu-height 30
      ac-ignore-case nil
      ac-use-quick-help t
      ac-quick-help-prefer-pos-tip t
      ac-use-fuzzy t
      ac-fuzzy-enable t
      ac-dwim t)
; to enable auto-complete globally
; (global-auto-complete-mode t)
(setq ac-trigger-commands
      (cons 'backward-delete-char-untabify ac-trigger-commands))

;;===========================================================================
;; show the menu
;;===========================================================================
(setq ac-show-menu-immediately-on-auto-complete t)

;;===========================================================================
; set the default sources for auto completion
;;===========================================================================
;; (setq-default ac-sources '(ac-source-dictionary
;;                            ac-source-words-in-buffer
;;                            ac-source-words-in-all-buffer
;;                            ac-source-functions
;;                            ac-source-yasnippet
;;                            ac-source-abbrev
;;                            ac-source-words-in-same-mode-buffers))

;; define the same using function
(defvar aqua/ac-sources-default
  "Default ac completion sources."
  '(ac-source-dictionary
    ac-source-words-in-buffer
    ac-source-words-in-all-buffer
    ac-source-functions
    ac-source-yasnippet
    ac-source-abbrev
    ac-source-words-in-same-mode-buffers))

(setq-default ac-sources aqua/ac-sources-default)

;;===========================================================================
; add on for individual modes if any
;;===========================================================================
(defun aqua/add-sources-for-prog ()
  "Add ac-sources for each programming-mode."
  (let ((add-source
                 (lambda (s)
                   (setq ac-sources (append `(,s) aqua/ac-sources-default)))))
    (cl-case major-mode
      (go-mode      (funcall add-source 'ac-source-go))
      (haskell-mode (add-to-list 'ac-sources 'ac-source-ghc-mod)))))

;;===========================================================================
; add to programming hooks
;;===========================================================================
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)


;;
;; for disabling auto-complete mode for a specific mode
; (defadvice auto-complete-mode (around disable-auto-complete-for-progname)
;   (unless (eq major-mode 'progname-mode) ad-do-it))
; (ad-activate 'auto-complete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avoid competing with org-mode templates.                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (make-local-variable 'ac-stop-words)
;;             (loop for template in org-structure-template-alist do
;;                   (add-to-list 'ac-stop-words
;;                                (concat "<" (car template))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add lisp autocomplete-support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ac-complete-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ac-complete-config.el ends here
