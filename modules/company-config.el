;;; package  --- company-config.el
;;; -*- coding: utf-8 -*-
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
(require 'company)               ;; company mode
(require 'company-quickhelp)     ;; documentation popup for company
(require 'company-dict)          ;; ac-source-dictionary to company-mode
(require 'company-math)          ;; back-ends for for math unicode symbols and latex tags

;;
;;; Code:
;;;

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
      ; invert navigation direction if completion popup-isearch-match
      ; is displayed on top (happens near the bottom of windows)
      company-tooltip-flip-when-above t
      ;; additional options
      company-tooltip-limit 20                       ;; bigger popup window
      company-tooltip-minimum-width 50               ;; minimum width of tooltips inner area
      ;company-idle-delay 0.5                        ;; decrease delay before autocompletion popup shows
      company-idle-delay 0.125                       ;; provide completions only if needed
      company-echo-delay 0                           ;; remove annoying blinking
      company-transformers '(company-sort-by-occurrence)
      company-begin-commands '(self-insert-command)) ;; start autocompletion only after typing
;;
;; (auto-complete-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dabbrev-like company-mode back-end for code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load "company-dabbrev-code"
  ;; Search all other buffers
  ;; (setq company-dabbrev-code-other-buffers 'all)
  ;; if below is t it offers completions in comments and strings.
  (setq company-dabbrev-code-everywhere nil
        company-dabbrev-downcase 0)
  ;; Ignore case when collecting completion candidates.
  (setq company-dabbrev-code-ignore-case t
        company-dabbrev-ignore-case nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adds fuzzy matching to company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'company
  (company-flx-mode +1))

;;
; look for dictionary files
;;
(setq company-dict-dir (concat cache-dir "dict/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specify all the company frontends and backends to be included
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq company-frontends
  '(company-pseudo-tooltip-unless-just-one-frontend
    company-preview-if-just-one-frontend))

(setq-default company-backends
    '((
        company-yasnippet
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
;; helm interface for company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESC - exit evils insert state and also the popup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use function from core/aqua-methods
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<escape>") 'aqua-company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prevent space to complete automatically during company completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'company
  (define-key company-active-map (kbd "SPC") #'aqua/company-stop-on-space))

(defun aqua/company-stop-on-space ()
  "Stop auto-complete and input a space as workaround for a semantic issue."
  (interactive)
  (company-abort)
  (insert " "))

;;
; company math mode
;;
;; (with-eval-after-load 'company
;;   (add-to-list 'company-math-symbols-unicode)
;;   (add-to-list 'company-math-symbols-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-ispell toggle as needed
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

(defun company-ispell-setup ()
  "Setup the company ISPELL."
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    (if (and (boundp 'ispell-alternate-dictionary) ispell-alternate-dictionary)
      (setq company-ispell-dictionary ispell-alternate-dictionary))))

(add-hook 'org-mode-hook 'company-ispell-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for Company mode with YaSnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun company-mode/backend-with-yas (backend)
  "Company YaSnippet Integration."
  (interactive)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  documentation popup for company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(company-quickhelp-mode 1)

;; unset M-h key
;; (global-set-key (kbd "M-h") nil)
(with-eval-after-load 'company
  (company-quickhelp-mode 1)
  (setq company-quickhelp-use-propertized-text t)
  (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight annotation when selected, match the search face when selected
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-background 'company-tooltip-annotation-selection
                     (face-background 'company-tooltip-selection))

(set-face-foreground 'company-tooltip-search-selection
                     (face-foreground 'company-tooltip-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A company-complete alternative that tries much harder to find completions.
;; If none of the current completions look good, call the command again to try
;; the next backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-try-hard)      ; get all completions from company backends


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode and YASnippet step on each other toes. These functions are  ;;
;; to help expected TAB function. Attempt these actions, and do the         ;;
;; first one that works.                                                    ;;
;; 1. expand yas snippet                                                    ;;
;; 2. auto complete with company                                            ;;
;; 3. indent                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-expansion ()
  "Check the expansion lists."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  "Fallback behavior for YASnippets."
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "Using TAB for indentation or completion."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; altering the key-maps of company and yas-minor modes
(defun bind-tab-properly ()
  "Binds tab to tab-indent-or-complete, overwriting yas and company bindings."
  (interactive)
  ;;overwrite yas and company tab mappings
  (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete)
  (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map [tab] 'tab-indent-or-complete)
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete))

(add-hook 'company-mode-hook 'bind-tab-properly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'company-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; company-config.el ends here
