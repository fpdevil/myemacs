;;; package --- customized settings under aqua-internals.el
;;;
;;; Commentary:
;;;
;;; filename.  : ~/.emacs.d/aqua-internals.el
;;; description: contains general Emacs customizations and custom functions
;;;              which cannot be placed anywhere.  Any customized settings
;;;              for the Emacs (aquamacs) or any packages or internal(s) may
;;;              be placed here and the same will be loaded during bootstrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code:
;;;
(global-set-key (kbd "<menu>") 'nil)

;----------------------------------------------------------------------------
;;                          some customizations
;----------------------------------------------------------------------------
(setq user-full-name "Sampath Singamsetty"
      user-mail-address "Singamsetty.Sampath@gmail.com")

;----------------------------------------------------------------------------
;; garbage collection setup for aquamacs
;; Start garbage collection every 50MB to improve Emacs performance
;; warn when opening files bigger than 100MB
;----------------------------------------------------------------------------
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)


;----------------------------------------------------------------------------
;; Enable Windows-like bindings (C-x and C-v)
;----------------------------------------------------------------------------
(cua-mode 1)


;----------------------------------------------------------------------------
;; typing related
;----------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p) ;; Don’t make me type yes or no to boolean interface questions
(setq echo-keystrokes 0.1)        ;; Show the modifier combinations I just typed


;----------------------------------------------------------------------------
;; OSX hosts are called hostname.localconfig or hostname.local, correct it
;----------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (setq system-name (car (split-string system-name "\\."))))

;----------------------------------------------------------------------------
;; record changes in window configurations
;----------------------------------------------------------------------------
(when (fboundp 'winner-mode)
  (winner-mode 1))


;----------------------------------------------------------------------------
;; recent files list
;----------------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)
;; change the recentf file location to cache directory
(setq recentf-save-file (expand-file-name "recentf" (concat (getenv "HOME") "/.emacs.d/cache")))
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 'never)
(recentf-load-list)

(setq truncate-lines t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Quit Aquamacs? "))
            kill-emacs-query-functions))

(put 'upcase-region 'disabled nil)

;----------------------------------------------------------------------------
;; file backup and saving Emacs sessions
;----------------------------------------------------------------------------
(setq desktop-save nil) ;; save without asking
;(setq desktop-restore-eager 10)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electric pair mode (currently disabled)                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (electric-pair-mode 1)
; (defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
; (defun markdown-add-electric-pairs ()
;   (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
;   (setq-local electric-pair-text-pairs electric-pair-pairs))
; (add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)


;----------------------------------------------------------------------------
;; disable electric indent mode to prevent auto indentation
;----------------------------------------------------------------------------
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

;----------------------------------------------------------------------------
;; custom keybinding stuff
;----------------------------------------------------------------------------
(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
; (global-set-key (kbd "RET") 'newline-and-indent)
; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
; (global-set-key (kbd "M-/") 'hippie-expand)
; (global-set-key (kbd "C-c C-k") 'compile)
; (global-set-key (kbd "C-x g") 'magit-status)
;----------------------------------------------------------------------------


;----------------------------------------------------------------------------
;; get default timestamp format
;----------------------------------------------------------------------------
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

;----------------------------------------------------------------------------
;; for latex editing
;----------------------------------------------------------------------------
(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)


;----------------------------------------------------------------------------
;; colorize the output of the compilation mode.
;----------------------------------------------------------------------------
(defun aqua-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))


(require 'compile)
(setq compilation-ask-about-save nil            ;; save before compile
      compilation-always-kill t                 ;; kill old process before starting new
      compilation-scroll-output 'first-error)   ;; scroll to first error automatically

;; colorize the compilation mode
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'aqua-colorize-compilation-buffer)


;----------------------------------------------------------------------------
;; Programming Mode Hooks
;; make sure things like FIXME and TODO are highlighted so they stand out
;----------------------------------------------------------------------------
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
;; Spell checker - setting location for aspell
;----------------------------------------------------------------------------
(when (and (eq system-type 'darwin) (executable-find "aspell"))
    (setq ispell-program-name (executable-find "aspell")))


;----------------------------------------------------------------------------
;; display world times of interest with M-x display-time-world
;----------------------------------------------------------------------------
(setq display-time-world-list
      '(("Asia/India" "India")
        ("America/Chicago" "Chicago")
        ("America/Los_Angeles" "Los Angeles")))

;----------------------------------------------------------------------------
;; abbrev configuration
;----------------------------------------------------------------------------
(add-hook 'text-mode-hook 'abbrev-mode)

;----------------------------------------------------------------------------
;; tab handling
;----------------------------------------------------------------------------
(global-set-key [tab] 'indent-or-expand)
(defun indent-or-expand ()
  "Either indent according to mode, or expand the word preceding the point."
  (interactive)
  (if (or
       (eq last-command 'self-insert-command)
       (eq last-command 'dabbrev-expand))
      (progn
        (setq this-command 'dabbrev-expand)
        (deabbrev-expand nil))
    (indent-according-to-mode)))

(when (eq system-type 'darwin)
  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

(message "Loaded the aquq-internals...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'aqua-internals)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-internals.el ends here