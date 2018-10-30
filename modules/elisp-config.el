;;; package --- elisp configuration settings
;;;
;;; Commentary:
;;;
;;; Filename    : elisp-config.el
;;; Description : A major mode elisp language support in Emacs
;;;
;;; elisp code for elisp language support and handling
;;;
;;; Code:
;;;
;;;==========================================================================

(require-package 'elisp-format)             ;; elisp code formatter

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\.bmk\\'"   . emacs-lisp-mode))

(add-hook 'emacs-lisp-mode-hook 'global-prettify-symbols-mode)

;;{{{ mode name shortening
(defun shorten-emacs-lisp-mode-name ()
  "Shorten the mode name."
  (setq mode-name "Elisp"))
(add-hook 'emacs-lisp-mode-hook #'shorten-emacs-lisp-mode-name)

;;}}}


;;{{{ display context sensitive help with eldoc for elisp-mode
(require-package 'elisp-slime-nav)
(defun imenu-elisp ()
  "Handle sections of elisp under imenu."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook #'imenu-elisp)

(defun my-lisp-hook ()
  "For slime and eldoc enabling."
  (progn
    (elisp-slime-nav-mode)
    (eldoc-mode)))

(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'my-lisp-hook)
(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)

;; enable doc for ielm buffer
(eval-after-load 'ielm
  '(progn
     (add-hook 'inferior-emacs-lisp-mode-hook
               (lambda ()
                 (eldoc-mode)))))

;; disable evil in the ielm
(after "ielm"
  (add-hook 'ielm-mode-hook 'turn-off-evil-mode)
  (evil-set-initial-state 'ielm 'emacs))

;;}}}

;;{{{ flycheck
(after "flycheck"
       ;; do not show errors for require statements
       (setq-default flycheck-emacs-lisp-load-path 'inherit)
       (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
;;}}}

;;{{{ company elisp backend for auto completion support
(after "company"
       (add-hook 'emacs-lisp-mode-hook
                 (lambda ()
                   (set (make-local-variable 'company-backends) '(company-elisp))))
       (add-hook 'ielm-mode-hook
                 (lambda ()
                   (set (make-local-variable 'company-backends) '(company-elisp)))))
;;}}}

;;{{{ @see http://blog.urth.org/2011/06/02/flymake-versus-the-catalyst-restarter/
(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.  This is a replacement for `flymake-create-temp-inplace'.  The difference is that it gives a file name in `temporary-file-directory' instead of the same directory as FILE-NAME.  For the use of PREFIX see that function.  Note that not making the temporary file in another directory \(like here) will not if the file you are checking depends on relative paths to other files \(for the type of check which flymake does)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext)))
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))
;;}}}

;;{{{ read only mode for the Emacs library lisp files
(defun set-elisp-libs-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'set-elisp-libs-readonly)

;;}}}

;;{{{ flymake - do not use elisp lint
(defun flymake-elisp-init ()
  "Do not use the elisp lint."
  (unless (or (string-match "^ " (buffer-name)) (aqua/is-buffer-file-temp))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval"
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (condition-case data
                  (scan-sexps (point-min) (point-max))
                (scan-error
                 (goto-char(nth 2 data))
                 (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                file (line-number-at-pos)))))))))
        local-file)))))

;;}}}

;;{{{ hippie-expand
(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;;}}}

;;{{{ elisp required features
(defun elisp-mode-hook-setup ()
  "Enable necessary features for elisp."
  (unless (aqua/is-buffer-file-temp)
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t)
      ;; (turn-on-eldoc-mode)
      (eldoc-mode))
    (enable-paredit-mode)
    (rainbow-delimiters-mode t)
    (set-up-hippie-expand-for-elisp)
    (flymake-mode)
    (checkdoc-minor-mode)))
(add-hook 'emacs-lisp-mode-hook          #'elisp-mode-hook-setup)
;;}}}

;;{{{ search lisp doc under the cursor
(defun lispdoc ()
  "Search lispdoc.com for SYMBOL, which is by default the symbol currently under the cursor."
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

;;}}}

;;{{{ for ielm Emacs lisp repl

(after "auto-complete"
       (require 'ac-slime)
       (add-hook 'slime-mode-hook 'set-up-slime-ac)
       (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
       '(add-to-list 'ac-modes 'slime-repl-mode 'emacs-lisp-mode)

       (defun ielm-auto-complete ()
         "Enable `auto-complete' support in the \\[ielm]."
         (setq ac-sources '(ac-source-functions
                            ac-source-variables
                            ac-source-features
                            ac-source-symbols
                            ac-source-words-in-same-mode-buffers))
         ;;(add-to-list 'ac-modes 'inferior-emacs-lisp-mode-hook)
        )

       (add-hook 'ielm-mode-hook (lambda () (auto-complete-mode -1)))
       ;;(add-hook 'ielm-mode-hook 'ielm-auto-complete)
       (add-hook 'emacs-lisp-mode-hook 'ielm-auto-complete)
       )
;;}}}

;;{{{ Automatically compile Emacs Lisp libraries
(require-package 'auto-compile)
(auto-compile-on-save-mode)
(setq auto-compile-display-buffer nil
      auto-compile-use-mode-line nil)
;;}}}


;;{{{ popup documentation at idle
(defun elisp-function-or-variable-quickhelp (symbol)
  "Display summary of function or variable at point for the SYMBOL.

Adapted from `describe-function-or-variable'."
  (interactive
   (let* ((v-or-f (variable-at-point))
          (found (symbolp v-or-f))
          (v-or-f (if found v-or-f (function-called-at-point))))
     (list v-or-f)))
  (if (not (and symbol (symbolp symbol)))
      (message "You didn't specify a function or variable")
    (let* ((fdoc (when (fboundp symbol)
                   (or (documentation symbol t) "Not documented.")))
           (fdoc-short (and (stringp fdoc)
                            (substring fdoc 0 (string-match "\n" fdoc))))
           (vdoc (when  (boundp symbol)
                   (or (documentation-property symbol 'variable-documentation t)
                       "Not documented as a variable.")))
           (vdoc-short (and (stringp vdoc)
                            (substring vdoc 0 (string-match "\n" vdoc)))))
      (and (require 'popup nil 'no-error)
           (popup-tip
            (or
             (and fdoc-short vdoc-short
                  (concat fdoc-short "\n\n"
                          (make-string 30 ?-) "\n" (symbol-name symbol)
                          " is also a " "variable." "\n\n"
                          vdoc-short))
             fdoc-short
             vdoc-short)
            :margin t)))))
;;}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'elisp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; elisp-config.el ends here
