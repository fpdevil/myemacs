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
;;;===========================================================================
(require 'company-quickhelp)     ;; documentation popup for company
(require 'company-dict)          ;; ac-source-dictionary to company-mode
(require 'company-math)          ;; back-ends for for math unicode symbols and latex tags

(when (eq dotemacs-completion-engine 'company)

  (defgroup emacs-company nil
    "Completion specific configuration options for company-mode."
    :group 'dotemacs
    :prefix 'dotemacs-company)
  ;; load the company library
  (require 'company)

  (setq company-auto-complete nil         ;;  this will accept highlighted item with SPC if t
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil                ;; case sensitive completion
        ;; invert navigation direction if completion popup-isearch-match
        ;; is displayed on top (happens near the bottom of windows)
        company-tooltip-flip-when-above t
        ;; additional options
        company-tooltip-limit 20                              ;; bigger popup window
        company-tooltip-minimum-width 50                      ;; minimum width of tool-tips inner area
        company-idle-delay 0.2                                ;; provide completions only if needed
        company-echo-delay 0                                  ;; remove annoying blinking
        company-transformers '(company-sort-by-occurrence)
        company-begin-commands '(self-insert-command)         ;; start autocompletion only after typing
        company-dict-dir (expand-file-name "dict" cache-dir)) ;; look for dictionary files

  ;; From Steve Purcell's configuration
  ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
  (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
    ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
    (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
        (setq ad-return-value nil)
      ad-do-it))

  ;; do not load company-mode for certain major modes
  (setq company-global-modes
        '(not
          eshell-mode
          comint-mode
          org-mode
          erc-mode
          gud-mode
          rcirc-mode
          minibuffer-inactive-mode))

  ;; dabbrev-like company-mode back-end for code. search all other buffers
  ;; (setq company-dabbrev-code-other-buffers 'all)
  ;; if the below is set t, it offers completions even in comments and strings.
  (setq company-dabbrev-code-everywhere nil
        company-dabbrev-downcase 0)
  ;; Ignore case when collecting completion candidates.
  (setq company-dabbrev-code-ignore-case t
        company-dabbrev-ignore-case nil)

  (unless (face-attribute 'company-tooltip :background)
    (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
    (set-face-attribute 'company-preview nil :background "black")
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
    (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))

    (when (executable-find "tern")
      (after "company-tern-autoloads"
        (add-to-list 'company-backends 'company-tern)))

  ;; add fuzzy matching to company, this has a big performance impact
  ;; (require-package 'company-flx)
  ;; (company-flx-mode)
  ;; (company-flx-mode +1)

  ;; helm interface for company-mode
  (after 'helm
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))

  ;; ESC - exit evils insert state and also the popup
  (define-key company-active-map (kbd "<escape>") 'aqua-company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort)

;; prevent space to complete automatically during company completions
  (define-key company-active-map (kbd "SPC") #'aqua/company-stop-on-space)

  (defun aqua/company-stop-on-space ()
    "Stop auto-complete and input a space as workaround for a semantic issue."
    (interactive)
    (company-abort)
    (insert " "))

;; company-ispell toggle as needed
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

  ;; Function for Company mode with YaSnippet
  (defun company-mode/backend-with-yas (backend)
    "Company YaSnippet Integration."
    (interactive)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (setq helm-yas-space-match-any-greedy t
        helm-yas-display-key-on-candidate t
        yas-wrap-around-region t
        yas-triggers-in-field t)

  ;; setup yasnippet prompt method
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-dropdown-prompt))

  ;; -- documentation popup for company
  ;; unset M-h key
  ;; (global-set-key (kbd "M-h") nil)
  (company-quickhelp-mode 1)
  (setq company-quickhelp-use-propertized-text t)
  (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)


  ;; -- highlight annotation when selected, match the search face when selected
  (set-face-background 'company-tooltip-annotation-selection
                       (face-background 'company-tooltip-selection))

  (set-face-foreground 'company-tooltip-search-selection
                       (face-foreground 'company-tooltip-search))

  ;; -- A company-complete alternative that tries much harder to find completions.
  ;;    == If none of the current completions look good, call the command again to try
  ;;    == the next backend
  (require 'company-try-hard)      ; get all completions from company backends

  ;; Company mode and YASnippet step on each other toes. These functions are
  ;; to help expected TAB function. Attempt these actions, and do the
  ;; first one that works.
  ;; 1. expand yas snippet
  ;; 2. auto complete with company
  ;; 3. indent
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

  ;; sort completions by usage frequency
  (require-package 'company-statistics)
  (setq company-statistics-file (expand-file-name "company-statistics-cache.el" cache-dir))
  (company-statistics-mode)

  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend))

  (setq company-backends
        '((
           company-yasnippet
           company-clang
           company-nxml
           company-gtags
           company-cmake
           company-c-headers
           company-files
           company-keywords
           company-capf
           company-dict
           company-dabbrev-code)
          (company-abbrev company-dabbrev)
          (company-math-symbols-latex company-math-symbols-unicode)))

  (setq company-backends (delete 'company-ropemacs company-backends))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'company-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; company-config.el ends here
