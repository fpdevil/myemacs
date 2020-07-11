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
;;;
;;; Code:
;;;

(when (eq dotemacs-completion-engine 'auto-complete)
  ;; required libraries
  (require 'auto-complete)
  (require 'auto-complete-config)
  ;; ac-slime provides a completion source for auto-complete
  ;; it provides pop-up documentation for completed symbols
  (require 'ac-slime)

  ;;--------------------------------------------------------------------------
  ;;** auto-completion (where company is not available)
  ;;** setting up auto-complete after yasnippet to avoid duplicate tab bindings
  ;;--------------------------------------------------------------------------
  (when (require 'auto-complete-config nil 'noerror)
    ;;(add-to-list 'ac-dictionary-directories (concat cache-dir "/ac-dict"))
    (setq ac-dictionary-files (concat cache-dir "/ac-dict"))
    (setq ac-comphist-file (concat cache-dir "/ac-comphist.dat"))
    (setq ac-auto-show-menu t)
    (setq ac-delay 0.4)
    (setq ac-use-quick-help t)
    (setq ac-quick-help-delay 0.2)
    ;; disable auto start as it always completes the first candidate
    ;; which could be quite annoying duing coding
    ;; (setq ac-auto-start nil)          ;; use M-Tab to trigger completion
    (setq ac-auto-start 1) ;; start after 1 chars
    (setq ac-use-menu-map t)
    (setq ac-menu-height 30)
    (setq ac-ignore-case nil)
    (setq ac-quick-help-prefer-pos-tip t)
    (setq ac-use-fuzzy t)
    (setq ac-fuzzy-enable t)
    (setq ac-dwim t) ;; do what i mean
    (setq ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////")))
    (setq ac-modes (append ac-modes '(org-mode))) ;; for org mode
    (ac-config-default))


  ;; show the menu immediately on auto-complete (default = t)
  ;; (setq ac-show-menu-immediately-on-auto-complete t)

  ;;**
  ;; <TAB> Control
  ;; Controls the operation of the TAB key. If t, hitting TAB always just
  ;; indents the  current line. If  nil, hitting TAB indents  the current
  ;; line if  point is at the  left margin or in  the line's indentation,
  ;; otherwise  it inserts  a "real"  TAB character.  If `complete',  TAB
  ;; first tries to indent the current  line, and if the line was already
  ;; indented, then try to complete the thing at point.
  (setq tab-always-indent 'complete)
  ;; completion keys
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  ;; (define-key ac-completing-map "\r" nil)
  ;; (define-key ac-completing-map "\t" 'ac-complete)
  ;; (global-set-key (kbd "C-M-Z") 'ac-fuzzy-complete)
  ;; set the trigger key so that it can work together with yasnippet on tab key,
  ;; if the word exists in yasnippet, pressing tab will cause yasnippet to
  ;; activate, otherwise, auto-complete will
  ;;(ac-set-trigger-key "<tab>")

  ;; This is a style added by sanityinc, purcell
  (add-to-list 'completion-styles 'initials t)

  ;; Stop completion-at-point from popping up completion buffers so eagerly
  (setq completion-cycle-threshold 5)

  ;; to enable auto-complete globally
  ;; (global-auto-complete-mode t)

  (dolist (command `(backward-delete-char-untabify delete-backward-char))
    (add-to-list 'ac-trigger-commands command))

  ;;-----------------------------------------------------------------------------
  ;; c-tab-always-indent:
  ;; If t, hitting TAB always just indents the current line.  If nil, hitting TAB
  ;; indents the current line if point is at the left margin or in the line's
  ;; indentation, otherwise it calls `c-insert-tab-function' to insert a `real'
  ;; tab character.
  ;; indent-for-tab-command:
  ;; Indent the current line or region, or insert a tab, as appropriate.
  ;;-----------------------------------------------------------------------------
  (setq c-tab-always-indent nil
        c-insert-tab-function 'indent-for-tab-command)

  ;;-----------------------------------------------------------------------------
  ;; hook AC into completion-at-point
  ;;-----------------------------------------------------------------------------
  (defun auto-complete-at-point ()
    "For aiding auto completion at a point."
    (when (and (not (minibufferp))
               (fboundp 'auto-complete-mode)
               auto-complete-mode)
      (auto-complete)))

  (defun never-indent ()
    "Set not to indent."
    (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

  (defun set-auto-complete-as-completion-at-point-function ()
    "Custom completion function."
    (setq completion-at-point-functions
          (cons 'auto-complete-at-point
                (remove 'auto-complete-at-point completion-at-point-functions))))

  (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

  ;;-----------------------------------------------------------------------------
  ;;-- linum mode for displaying line numbers for current buffer
  ;;-----------------------------------------------------------------------------
  (after 'linum
    (ac-linum-workaround))

  ;;-----------------------------------------------------------------------------
  ;;-- yasnippet integration
  ;;-----------------------------------------------------------------------------
  (after 'yasnippet
    (add-hook 'yas-before-expand-snippet-hook (lambda () (auto-complete-mode -1)))
    (add-hook 'yas-after-exit-snippet-hook (lambda () (auto-complete-mode t)))
    (defadvice ac-expand (before dotemacs activate)
      (when (yas-expand)
        (ac-stop))))

  ;;-----------------------------------------------------------------------------
  ;;-- define the ac sources using function
  ;;-----------------------------------------------------------------------------
  (defvar aqua/ac-sources-default
    '(
      ac-source-filename
      ac-source-files-in-current-dir
      ac-source-abbrev
      ac-source-dictionary
      ac-source-functions
      ac-source-yasnippet
      ac-source-words-in-buffer
      ac-source-words-in-all-buffer
      ac-source-words-in-same-mode-buffers
      )
    "Default ac completion sources."
    )
  (setq-default ac-sources aqua/ac-sources-default)

  (defun ac-start-use-sources (sources)
    (interactive)
    (let ((ac-sources sources))
      (call-interactively 'ac-start)))

  ;;-----------------------------------------------------------------------------
  ;;-- add on for individual modes if any
  ;;-----------------------------------------------------------------------------
  (defun aqua/add-sources-for-prog ()
    "Add ac-sources for each programming-mode."
    (let ((add-source
           (lambda (s)
             (setq ac-sources (append `(,s) aqua/ac-sources-default)))))
      (cl-case major-mode
        (go-mode (funcall add-source 'ac-source-go))
        (haskell-mode (add-to-list 'ac-sources
                                   'ac-source-ghc-mod)))))

  (aqua/add-sources-for-prog)


  ;;-----------------------------------------------------------------------------
  ;;** for disabling auto-complete mode for a specific mode
  ;;-----------------------------------------------------------------------------
  (defadvice auto-complete-mode (around disable-auto-complete-for-progname)
    (unless (eq major-mode 'progname-mode) ad-do-it))
  ;; (ad-activate 'auto-complete-mode)

  ;;----------------------------------------------------------------------------
  ;;-- add lisp auto complete support
  ;;----------------------------------------------------------------------------
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (after "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode 'emacs-lisp-mode))

  ;;----------------------------------------------------------------------------
  ;;-- flyspell mode breaks auto-complete mode without this.
  ;;----------------------------------------------------------------------------
  (ac-flyspell-workaround)

  ;;----------------------------------------------------------------------------
  ;;-- etags/ctags completion source for auto-complete
  ;;----------------------------------------------------------------------------
  (require-package 'ac-etags)
  (setq ac-etags-requires 1)
  (after 'etags
    (ac-etags-setup))

  ;;----------------------------------------------------------------------------
  ;;-- for disabling or removing a single source from ac-sources
  ;;----------------------------------------------------------------------------
  (defun ac-source-remove (source-removed-list)
    "Remove some SOURCE-REMOVED-LIST ac-source from ac-sources.

  Example:
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (ac-source-remove '(ac-source-capf ac-source-symbols))))"
    (interactive)
    ;; Use `remq' instead of `delq' which will remove element in global `ac-sources' too.
    (mapc (lambda (x) (setq-local ac-sources (remq x ac-sources)))
          source-removed-list))

  ;;----------------------------------------------------------------------------
  ;; ac triggering
  ;;----------------------------------------------------------------------------
  (defvar ac-trigger-edit-commands
    `(self-insert-command
      delete-backward-char
      backward-delete-char
      backward-delete-char-untabify)
    "*Trigger edit commands that specify whether `auto-complete' should start or not when `ac-completing'.")

  (after "cc-mode"
    '(progn
       (dolist (command `(c-electric-backspace
                          c-electric-backspace-kill))
         (add-to-list 'ac-trigger-commands command)
         (add-to-list 'ac-trigger-edit-commands command))))

  (after "autopair"
    '(progn
       (dolist (command `(autopair-insert-or-skip-quote
                          autopair-backspace
                          autopair-extra-skip-close-maybe))
         (add-to-list 'ac-trigger-commands command))

       (defun ac-trigger-command-p ()
         "Return non-nil if `this-command' is a trigger command."
         (or
          (and
           (memq this-command ac-trigger-commands)
           (let* ((autopair-emulation-alist nil)
                  (key (this-single-command-keys))
                  (beyond-autopair (or (key-binding key)
                                       (key-binding (lookup-key local-function-key-map key)))))
             (memq beyond-autopair ac-trigger-edit-commands)))
          (and ac-completing
               (memq this-command ac-trigger-edit-commands))))))

  )

(provide 'ac-complete-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ac-complete-config.el ends here
