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
;;; @https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-company.el
;;; elisp code for standard auto-completion configuration with company-mode
;;;
;;; Code:
;;;
;;;===========================================================================
(require-package 'company)
(require 'company)               ;; company mode
(require 'company-quickhelp)     ;; documentation popup for company
(require 'company-dict)          ;; ac-source-dictionary to company-mode
(require 'company-math)          ;; back-ends for for math unicode symbols and latex tags


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            company mode (for company based completions)                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use company-mode in all buffers globally
(add-hook 'after-init-hook 'global-company-mode)


;;-----------------------------------------------------------------------------
;; company options - frontends and backends
;;-----------------------------------------------------------------------------
(after 'company
  '(progn
    (lazy-init
      ;; sort the completions by their usage frequency
      (require-package 'company-statistics)
      (setq company-statistics-file (expand-file-name "company-statistics-cache.el" cache-dir))
      (company-statistics-mode))

     (setq company-frontends
           '(company-pseudo-tooltip-unless-just-one-frontend
             company-preview-if-just-one-frontend
             company-echo-metadata-frontend))

     (setq company-backends
           '((
              company-yasnippet
              ;; company-nxml
              ;; company-gtags
              company-cmake
              company-c-headers
              company-files
              company-keywords
              company-capf
              company-dict
              company-dabbrev-code)
       (company-abbrev company-dabbrev)
       (company-math-symbols-latex company-math-symbols-unicode)))

     ;; remove unused backends
     (setq company-backends (delete 'company-ropemacs company-backends))

     (setq company-auto-complete nil         ;;  this will accept highlighted item with SPC if t
           company-minimum-prefix-length 2
           company-tooltip-align-annotations t
           company-selection-wrap-around t
           company-show-numbers t
           company-require-match nil
           company-etags-ignore-case t

           company-dabbrev-downcase nil
           company-dabbrev-ignore-case t                ;; case sensitive completion

           ;; invert navigation direction if completion popup-isearch-match
           ;; is displayed on top (happens near the bottom of windows)
           company-tooltip-flip-when-above t
           ;; additional options
           company-tooltip-limit 20                             ;; bigger popup window
           company-tooltip-minimum-width 50                     ;; minimum width of tool-tips inner area
           ;; company-idle-delay 0.5                            ;; decrease delay before autocompletion popup shows
           company-idle-delay 0.2                               ;; provide completions only if needed
           company-echo-delay 0                                 ;; remove annoying blinking
           company-clang-insert-arguments nil
           company-transformers '(company-sort-by-occurrence)
           company-begin-commands '(self-insert-command))       ;; start autocompletion only after typing
           company-dict-dir (expand-file-name "dict" cache-dir) ;; look for dictionary files

           ;; From Chen's configuration
           ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
           (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
             ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
             (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
                 (setq ad-return-value nil)
               ad-do-it))

           ;; Tern mode javascript completion
           (when (executable-find "tern")
             (after "company-tern-autoloads"
               (add-to-list 'company-backends 'company-tern)))

           ;; do not use company completion for certain modes like xml, xsl, nxml modes
           (setq company-global-modes
                 '(not
                   eshell-mode
                   comint-mode
                   erc-mode
                   gud-mode
                   rcirc-mode
                   minibuffer-inactive-mode
                   xml-mode
                   nxml-mode
                   inferior-emacs-lisp-mode
                   xsl-mode
                   xslt-process-mode))))

;;-----------------------------------------------------------------------------
;; adds fuzzy matching to company
;;-----------------------------------------------------------------------------
(after 'company
  (company-flx-mode +1))

;;-----------------------------------------------------------------------------
;; ESC - exit evils insert state and also the popup
;;-----------------------------------------------------------------------------
;; use function from core/aqua-methods
(after 'company
  (define-key company-active-map (kbd "<escape>") 'aqua-company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort))

;;-----------------------------------------------------------------------------
;; prevent space to complete automatically during company completions
;;-----------------------------------------------------------------------------
(after 'company
  (define-key company-active-map (kbd "SPC") #'aqua/company-stop-on-space))

(defun aqua/company-stop-on-space ()
  "Stop auto-complete and input a space as workaround for a semantic issue."
  (interactive)
  (company-abort)
  (insert " "))

;;-----------------------------------------------------------------------------
;; company math mode
;;-----------------------------------------------------------------------------
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

;; do not turn on company ispell for org mode
(add-hook 'org-mode-hook 'company-ispell-setup)

(after 'company-etags
  '(progn
     ;; insert major-mode not inherited from prog-mode
     ;; to make company-etags work
     (add-to-list 'company-etags-modes 'web-mode)
     (add-to-list 'company-etags-modes 'lua-mode)))

;;-----------------------------------------------------------------------------
;;                  documentation popup for company
;;-----------------------------------------------------------------------------
;(company-quickhelp-mode 1)

;; unset M-h key
;; (global-set-key (kbd "M-h") nil)
(after 'company
  (company-quickhelp-mode 1)
  (setq company-quickhelp-use-propertized-text t)
  (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;;-----------------------------------------------------------------------------
;; highlight annotation when selected, match the search face when selected
;;-----------------------------------------------------------------------------
(after 'company
  (set-face-background 'company-tooltip-annotation-selection
                       (face-background 'company-tooltip-selection))
  (set-face-foreground 'company-tooltip-search-selection
                       (face-foreground 'company-tooltip-search)))

;;-----------------------------------------------------------------------------
;; A company-complete alternative that tries much harder to find completions.
;; If none of the current completions look good, call the command again to try
;; the next backend
;;-----------------------------------------------------------------------------
(require 'company-try-hard)      ; get all completions from company backends

;;----------------------------------------------------------------------------
;; add company-yasnippet support for all the company backends
;; https://github.com/syl20bnr/spacemacs
;;----------------------------------------------------------------------------
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Company YaSnippet Integration."
  (if (or (not company-mode/enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;; uncomment the below to add :with company-yasnippet to all backends
;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

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

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (do-yas-expand)))
      (if company-candidates
          (company-complete-selection)
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (yas-next-field))))
          (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

;; altering the key-maps of company and yas-minor modes
(defun bind-tab-properly ()
  "Binds tab to tab-indent-or-complete, overwriting yas and company bindings."
  (interactive)
  ;;overwrite yas and company tab mappings
  (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete)
  (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map [tab] 'tab-indent-or-complete)
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete))

;; (add-hook 'company-mode-hook 'bind-tab-properly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'company-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; company-config.el ends here
