;;; package  --- company and plugin configurations
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

(when (eq dotemacs-completion-engine 'company)

  (defgroup dotemacs-company nil
    "Configuration options for company-mode."
    :group 'dotemacs
    :prefix 'dotemacs-company)

  (defun company-config/push-company-backends-locally (backend)
    "Add the specified BACKEND to a local buffer."
    (make-local-variable 'company-backends)
    (push backend company-backends))

  (require 'company)

  ;;** use company-mode in all buffers globally
  (add-hook 'after-init-hook 'global-company-mode)

  ;;** company backends and frontends
  ;;   "Modes for which `company-mode' mode is turned on by
  ;; `global-company-mode'.  If nil, means no modes.  If t, then all major modes
  ;; have it turned on.  If a list, it should be a list of `major-mode' symbol
  ;; names for which `company-mode' should be automatically turned on.  The sense
  ;; of the list is negated if it begins with `not'.  For example: (c-mode
  ;; c++-mode) means that `company-mode' is turned on for buffers in C and C++
  ;; modes only.  (not message-mode) means that `company-mode' is always turned
  ;; on except in `message-mode' buffers."
  ;; (setq company-global-modes '(
  ;;                             c-mode
  ;;                             c++-mode
  ;;                             ;; Add both modes in, given not sure what is the
  ;;                             ;; real relation between them.
  ;;                             latex-mode
  ;;                             LaTeX-mode
  ;;                             ))

  ;;*** frontends
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend))

  ;;*** backends
  ;;(add-to-list 'company-backends 'company-math-symbols-latex)
  ;;(add-to-list 'company-backends 'company-gtags)
  ;;(add-to-list 'company-backends 'company-cmake)
  ;;(add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-etags)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-files)

  ;; remove unused backends
  (setq company-backends (delete 'company-ropemacs company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  (setq-default company-backends (remove 'company-eclim company-backends))

  (setq company-auto-complete nil ;;  this will accept highlighted item with SPC if t
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-show-numbers t

        ;; does not require to match a completion when I type
        company-require-match nil
        company-etags-ignore-case t

        ;; dabbrev backends should only look for candidates in buffers with the same major mode
        company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t
        ;; auto complete should preserve the original case as much as possible
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        ;; completions in comments and strings if non nil
        company-dabbrev-code-everywhere t

        ;; invert navigation direction if completion popup-isearch-match
        ;; is displayed on top (happens near the bottom of windows)
        company-tooltip-flip-when-above t
        ;; additional options
        company-tooltip-limit 15 ;; bigger popup window
        company-tooltip-minimum-width 50 ;; minimum width of tool-tips inner area
        ;; company-idle-delay 0.5              ;; decrease delay before completion popup shows
        company-idle-delay 0.2 ;; provide completions only if needed
        company-echo-delay 0   ;; remove annoying blinking
        company-clang-insert-arguments nil
        company-transformers '(company-sort-by-occurrence)
        ;; company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)

        ;; start autocompletion only after typing
        company-begin-commands '(self-insert-command))


  ;;** documentation popup for company
  ;; unset M-h key
  ;; (global-set-key (kbd "M-h") nil)
  (require 'company-quickhelp)
  (setq company-quickhelp-use-propertized-text t
        company-quickhelp-local-mode nil ;; pop's an annoying frame for help
        company-frontends (delq 'company-echo-metadata-frontend company-frontends)
        company-quickhelp-color-background "Purple"
        company-quickhelp-color-foreground "Yellow"
        company-quickhelp-delay 0.1
        company-quickhelp-max-lines nil
        )
  ;;'(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
  ;;(add-hook 'after-init-hook 'company-quickhelp-mode)
  (company-quickhelp-mode 1)

  ;;** port of ac-source-dictionary to company-mode
  ;;   plus annotation and documentation support
  (require 'company-dict)
  ;; if you want it available everywhere
  (add-to-list 'company-backends 'company-dict)
  ;; Where to look for dictionary files. Default is ~/.emacs.d/dict
  (setq company-dict-dir (expand-file-name "dict" cache-dir))

  ;;** fill column indicator
  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  ;;** adds fuzzy matching to company
  (company-flx-mode +1)

  ;;** prevent space to complete automatically during company completions
  (define-key company-active-map (kbd "SPC") #'aqua/company-stop-on-space)

  ;;** company math mode
  ;;   (add-to-list 'company-math-symbols-unicode)
  ;;   (add-to-list 'company-math-symbols-latex)

  ;; A company-complete alternative that tries much harder to find completions.
  ;; If none of the current completions look good, call the command again to try
  ;; the next backend
  ;; get all completions from company backends
  (require 'company-try-hard)

  ;; work-around for issues with fci-mode
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    "Turn off fci for the REST and IGNORE items."
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    "Turn off fci-mode for REST IGNORE."
    (when company-fci-mode-on-p (fci-mode 1)))

  (defun aqua/company-stop-on-space ()
    "Stop auto-complete and input a space as workaround for a semantic issue."
    (interactive)
    (company-abort)
    (insert " "))


  ;;** do not turn on company ispell for org and text modes
  (add-hook 'org-mode-hook 'company-ispell-setup)
  (add-hook 'text-mode-hook 'company-text-mode-hook)

  ;;** company ispell integration
  ;;** functions to toggle company-ispell as needed
  (defun toggle-company-ispell ()
    "Toggle company Ispell mode with M-x."
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

  (defun company-text-mode-hook ()
    "Set Company ISPELL for `text-mode'."
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    (setq company-ispell-dictionary (file-truename "~/.emacs.d/private/english-words.txt")))

  ;; setup for text-mode
  (add-hook 'text-mode-hook 'company-text-mode-hook)


  (after 'company-etags
    '(progn
       ;; insert major-mode not inherited from prog-mode
       ;; to make company-etags work
       (add-to-list 'company-etags-modes 'web-mode)
       (add-to-list 'company-etags-modes 'lua-mode)))


  ;;** highlight annotation when selected, match the search face when selected
  (set-face-background 'company-tooltip-annotation-selection
                       (face-background 'company-tooltip-selection))
  (set-face-foreground 'company-tooltip-search-selection
                       (face-foreground 'company-tooltip-search))


  ;;** add company-yasnippet support for all the company backends
  ;;   enable yasnippet everywhere
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  ;; ('company-backe nds (mapcar #'company-mode/backend-with-yas 'company-backends))

  (after 'yasnippet
    (setq company-backends
          (mapcar
           (lambda (backend)
             (if (and (listp backend) (member 'company-yasnippet backend))
                 backend
               (append (if (consp backend) backend (list backend))
                       '(:with company-yasnippet))))
           company-backends)))

  ;;** Company mode and YASnippet step on each other toes. These functions
  ;; are to help expected TAB function. Attempt these actions, and do the
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

  ;; company front-end with icons
  ;;(require-package 'company-box)
  ;;(add-hook 'company-mode-hook 'company-box-mode)

  (defun trigger-org-company-complete ()
    "Begins company-complete in org-mode buffer after pressing #+ chars."
    (interactive)
    (if (string-equal "#" (string (preceding-char)))
        (progn
          (insert "+")
          (company-complete))
      (insert "+")))
  ;; (eval-after-load 'org '(define-key org-mode-map
  ;;                          (kbd "+") 'trigger-org-company-complete))

  )


(provide 'company-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; company-config.el ends here
