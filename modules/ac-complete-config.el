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
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-completion (where company is not available)                         ;;
;; setting up autocomplete after yasnippet to avoid duplciate tab bindings  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories
    (concat (getenv "HOME") "/.emacs.d/vendor/auto-complete/dict"))
  (setq ac-comphist-file
    (concat (getenv "HOME") "/.emacs.d/cache/ac-comphist.dat"))
  (ac-config-default)
  )

(setq ac-auto-show-menu t
      ac-delay 0.6
      ac-quick-help-delay 0.9
      ac-use-fuzzy t
      ac-fuzzy-enable t
      ac-dwim t)
; to enable auto-complete globally
; (global-auto-complete-mode t)
; (setq ac-sources '(ac-source-yasnippet
;                    ac-source-abbrev
;                    ac-source-words-in-same-mode-buffers))
;; show the menu
(setq ac-show-menu-immediately-on-auto-complete t)

;;
;; for disabling auto-complete mode for a specific mode
; (defadvice auto-complete-mode (around disable-auto-complete-for-progname)
;   (unless (eq major-mode 'progname-mode) ad-do-it))
; (ad-activate 'auto-complete-mode)


(provide 'ac-complete-config)

;;; ac-complete-config.el ends here
