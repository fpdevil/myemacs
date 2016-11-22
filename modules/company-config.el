;;; package  --- company-config.el
;;;
;;; Commentary :
;;;
;;; Filename   : company-config.el
;;; Description: Modular in-buffer completion framework for Emacs
;;;              Company is a text completion framework for Emacs.
;;;
;;;
;;; elisp code for standard auto-completion configuration with company-mode
;;;===========================================================================
(require 'company)                  ;; company mode
(require 'company-quickhelp)        ;; documentation popup for company
(require 'company-dict)             ;; ac-source-dictionary to company-mode
(require 'company-math)             ;; back-ends for for math unicode symbols and latex tags

;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            company mode (for company based completions)                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; use company-mode in all buffers globally
;;
(add-hook 'after-init-hook 'global-company-mode)

;;
; company options and backends
;;
(setq company-auto-complete t)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-dabbrev-downcase 0)
; invert navigation direction if completion popup-isearch-match
; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
;; additional options
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay 0.0)                        ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;;
(auto-complete-mode 1)


;;
; look for dictionary files
;;
(setq company-dict-dir (concat user-emacs-directory "dict/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specify all the company backends to be included                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default company-backends
    '((company-yasnippet
       ; company-ispell
       company-files
       company-keywords
       company-capf
       company-dict
       company-dabbrev-code)
       (company-abbrev company-dabbrev)
       (company-math-symbols-latex company-math-symbols-unicode)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm interface for company-mode                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  documentation popup for company                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(company-quickhelp-mode 1)
(setq company-quickhelp-use-propertized-text t)


(provide 'company-config)

;;; company-config.el ends here
