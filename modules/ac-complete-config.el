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
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-completion (where company is not available)                         ;;
;; setting up autocomplete after yasnippet to avoid duplicate tab bindings  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories
    (concat (getenv "HOME") "/.emacs.d/vendor/auto-complete/dict"))
  (setq ac-comphist-file
    (concat (getenv "HOME") "/.emacs.d/cache/ac-comphist.dat"))
  (ac-config-default)
  )

(setq ac-auto-show-menu t
      ac-delay 0.2
      ac-quick-help-delay 1.0
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

;; show the menu
(setq ac-show-menu-immediately-on-auto-complete t)

(setq-default ac-sources '(ac-source-dictionary
                           ac-source-words-in-buffer
                           ac-source-words-in-all-buffer
                           ac-source-functions
                           ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-words-in-same-mode-buffers))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)


;;
;; for disabling auto-complete mode for a specific mode
; (defadvice auto-complete-mode (around disable-auto-complete-for-progname)
;   (unless (eq major-mode 'progname-mode) ad-do-it))
; (ad-activate 'auto-complete-mode)


(provide 'ac-complete-config)

;;; ac-complete-config.el ends here
