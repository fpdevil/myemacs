;;; package --- customized settings under aqua-internals.el
;;;
;;; Commentary:
;;;
;;; filename.  : ~/.emacs.d/aqua-internals.el
;;; description: contains general Emacs customizations and custom functions
;;;              which cannot be placed anywhere.  Any customized settings
;;;              for the Emacs (aquamacs) or any packages or internal(s) may
;;;              be placed here and the same will be loaded during bootstrap
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<menu>") 'nil)

;; a variable for holding if system is mac os x
(setq *is-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *emacs25* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 25))) )
(setq *no-memory*
      (cond
       (*is-mac*
        (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
       (*linux* nil)
       (t nil)))

(setq *emacs24old*
      (or (and (= emacs-major-version 24) (= emacs-minor-version 3))
          (not *emacs24*)))

;;----------------------------------------------------------------------------
;; garbage collection setup for aquamacs
;; Start garbage collection every 50MB to improve Emacs performance
;; warn when opening files bigger than 100MB
;;----------------------------------------------------------------------------
(defun my-minibuffer-setup-hook ()
  "Set GC threshold value for minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  "Set GC threshold."
  (setq gc-cons-threshold (* 64 1024 1024)))

;;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(message "Emacs Version: %d%s%d"
         emacs-major-version
         version-separator
         emacs-minor-version)

;; for text enrichment
(eval-after-load "enriched"
                 '(defun enriched-decode-display-prop (start end &optional param)
                    (list start end)))

;;----------------------------------------------------------------------------
;; Highlight and overwrite selected regions (CUA mode)
;; Enable Windows-like bindings (C-x and C-v)
;;----------------------------------------------------------------------------
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(setq cua-enable-modeline-indications t)
(setq cua-remap-control-v nil)
(setq cua-remap-control-z nil)

;;----------------------------------------------------------------------------
;; some defaults to be set
;;----------------------------------------------------------------------------
;; delete a selection with a single key-press
(delete-selection-mode t)
;; enable transient mode
(transient-mark-mode t)
;; auto revert
(global-auto-revert-mode t)
;; pcomplete
(setq pcomplete-ignore-case t)
;; indentation
(setq default-indent-tabs-mode nil)
;; lock files
(setq create-lockfiles nil)
;; ignore bell
(setq ring-bell-function 'ignore)

;;----------------------------------------------------------------------------
;; typing related
;;----------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)    ;; Don’t make me type yes or no to boolean interface questions
;; (defalias 'list-buffers 'ibuffer) ;; always use ibuffer (For C-x C-b)
(setq echo-keystrokes 0.1)           ;; Show the modifier combinations I just typed

;;----------------------------------------------------------------------------
;; OSX hosts are called hostname.localconfig or hostname.local, correct it
;;----------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (setq system-name (car (split-string system-name "\\."))))

;(defcustom hostname nil)
;
;(setq hostname
;  (replace-regexp-in-string "\\`[ \t\n]*" ""
;    (replace-regexp-in-string "[ \t\n]*\\'" ""
;      (shell-command-to-string "hostname"))))

;;----------------------------------------------------------------------------
;; record changes in window configurations
;; Enable undo on window configuration: C-c left (undo) and C-c right (redo)
;;----------------------------------------------------------------------------
(when (fboundp 'winner-mode) (winner-mode 1))
(add-hook 'after-init-hook 'winner-mode)

;;----------------------------------------------------------------------------
;; recent files list
;;----------------------------------------------------------------------------
(require 'recentf)
(recentf-mode t)
;; change the recentf file location to cache directory
(setq recentf-save-file (expand-file-name "recentf" (concat (getenv "HOME") "/.emacs.d/cache")))
(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 500)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 'never)
(recentf-load-list)
(run-with-idle-timer 600 t #'recentf-save-list)

(setq truncate-lines t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

;;----------------------------------------------------------------------------
;; prompt before closing Emacs
;;----------------------------------------------------------------------------
;; prompt while exiting in any way
;; (setq kill-emacs-query-functions
;;       (cons (lambda () (yes-or-no-p "Quit Aquamacs? "))
;;             kill-emacs-query-functions))

;; prompt while exiting with C-x C-c
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
;; check only if in gui mode
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(put 'upcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; file backup and saving Emacs sessions
;;----------------------------------------------------------------------------
(setq desktop-save nil)                               ; save without asking
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;----------------------------------------------------------------------------
;; electric pair mode (currently disabled)                                 ;;
;;----------------------------------------------------------------------------
; (electric-pair-mode 1)
; (defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
; (defun markdown-add-electric-pairs ()
;   (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
;   (setq-local electric-pair-text-pairs electric-pair-pairs))
; (add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)

;;----------------------------------------------------------------------------
;; disable electric indent mode to prevent auto indentation
;;----------------------------------------------------------------------------
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

;;----------------------------------------------------------------------------
;; custom keybinding stuff
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;----------------------------------------------------------------------------
;; get default timestamp format
;;----------------------------------------------------------------------------
(defun timestamp ()
  "Latest timestamp."
  (interactive)
  (insert (format-time-string "%d.%m.%Y, %H:%M:%S")))

(defun my-count-words-region (posBegin posEnd)
  "POSBEGIN POSEND Print number of words and chars in region."
  (interactive "r")
  (message "Counting …")
  (save-excursion
    (let (wordCount charCount)
      (setq wordCount 0)
      (setq charCount (- posEnd posBegin))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\w+\\W*" posEnd t))
        (setq wordCount (1+ wordCount)))
      (message "Words: %d. Chars: %d." wordCount charCount)
      )))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))


(defun unfill-region (start end)
  "START END Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;----------------------------------------------------------------------------
;; for latex editing
;;----------------------------------------------------------------------------
(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;;----------------------------------------------------------------------------
;; colorize the output of the compilation mode.
;;----------------------------------------------------------------------------
(defun aqua-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  (when (eq major-mode 'compilation-mode)
   (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil            ;; save before compile
      compilation-always-kill t                 ;; kill old process before starting new
      compilation-scroll-output 'first-error)   ;; scroll to first error automatically

(add-hook 'compilation-filter-hook #'aqua-colorize-compilation-buffer)

;;----------------------------------------------------------------------------
;; Programming Mode Hooks
;; make sure things like FIXME and TODO are highlighted so they stand out
;;----------------------------------------------------------------------------
(defun add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO."
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

(add-hook 'prog-mode-hook #'add-watchwords)

(defun annotate-todo ()
  "Put a fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

;----------------------------------------------------------------------------
;; display world times of interest with M-x display-time-world
;----------------------------------------------------------------------------
(setq display-time-world-list
      '(("Asia/India" "India")
        ("America/Chicago" "Chicago")
        ("America/Los_Angeles" "Los Angeles")))

;;----------------------------------------------------------------------------
;; abbrev configuration
;;----------------------------------------------------------------------------
(add-hook 'text-mode-hook 'abbrev-mode)

(when (eq system-type 'darwin)
  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; os x specific settings
(when (string-equal system-type "darwin")
  ;; Don't make new frames when opening a new file with Emacs
  (setq ns-pop-up-frames nil)
  ;; set the Fn key as the hyper key
  (setq ns-function-modifier 'hyper)
  ;; Not going to use these commands
  (put 'ns-print-buffer 'disabled t)
  (put 'suspend-frame 'disabled t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'aqua-internals)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-internals.el ends here
