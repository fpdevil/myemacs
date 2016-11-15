;;; package  --- company-config.el
;;;
;;; Commentary :
;;;
;;; Filename   : company-config.el
;;; Description: Modular in-buffer completion framework for Emacs
;;;              Company is a text completion framework for Emacs.
;;;              elisp code for customizing the company settings
;;;==============================================================
(require 'company)              ;; company mode
(require 'company-quickhelp)    ;; company popup

;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            company mode (for company based completions)                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
(setq company-auto-complete t)
(setq company-echo-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-selection-wrap-around t)
; invert navigation direction if completion popup-isearch-match
; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
; no delay for company suggestions
(setq company-idle-delay 0)
(auto-complete-mode 1)
; company backends
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
; helm interface for company-mode
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  documentation popup for company                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(company-quickhelp-mode 1)

(provide 'company-config)

;;; company-config.el ends here
