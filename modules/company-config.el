;;; package  --- company-config.el
;;;
;;; Commentary:
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
(setq company-auto-complete t
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-selection-wrap-around t
      company-show-numbers t
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-code-ignore-case nil
      company-dabbrev-downcase 0
      ; invert navigation direction if completion popup-isearch-match
      ; is displayed on top (happens near the bottom of windows)
      company-tooltip-flip-when-above t
      ;; additional options
      company-tooltip-limit 20                       ;; bigger popup window
      ;company-idle-delay 0.5                        ;; decrease delay before autocompletion popup shows
      company-idle-delay 0.5                         ;; provide completions only if needed
      company-echo-delay 0                           ;; remove annoying blinking
      company-transformers '(company-sort-by-occurrence)
      company-begin-commands '(self-insert-command)) ;; start autocompletion only after typing
;;
(auto-complete-mode 1)


;;
; adds fuzzy matching to company
;;
(with-eval-after-load 'company
  (company-flx-mode +1))

;;
; look for dictionary files
;;
(setq company-dict-dir (concat user-emacs-directory "dict/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specify all the company backends to be included                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default company-backends
    '((company-yasnippet
       company-elisp
       company-clang
       company-nxml
       company-gtags
       company-cmake
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
;; ESC - exit evils insert state and also the popup                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use function from core/aqua-methods
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<escape>") 'aqua-company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort))

;;
; company math mode
;;
;; (with-eval-after-load 'company
;;   (add-to-list 'company-math-symbols-unicode)
;;   (add-to-list 'company-math-symbols-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-ispell toggle as needed                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun switch-company-ispell ()
  "Toggle company Ispell mode."
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "disabling company-ispell mode"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "enabling company-ispell mode"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  documentation popup for company                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(company-quickhelp-mode 1)
(setq company-quickhelp-use-propertized-text t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight annotation when selected, match the search face when selected  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-background 'company-tooltip-annotation-selection
                     (face-background 'company-tooltip-selection))

(set-face-foreground 'company-tooltip-search-selection
                     (face-foreground 'company-tooltip-search))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some company front-ends                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; using defaults now

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'company-config)
;;; company-config.el ends here
