;;; package --- Aquamacs initialization file
;;; -*- coding: utf-8 -*-
;;;
;;;         ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;;         ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;;         █████╗  ██╔████╔██║███████║██║     ███████╗
;;;         ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;;         ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;;         ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;;
;;;
;;; Author    : Sampath Singamsetty <Singansetty.Sampath@gmail.com>
;;; URL       : https://github.com/singamsetty/myemacs
;;; Commentary:
;;;
;;; filename   : init.el
;;; description: initialization file for loading the necessary packages
;;;
;; This sets up the load path so that we can override it
;; Updated    : 01 Dec 2016
;;;===========================================================================
;;;
;;; Code:
;;;

;;
; utf-8 character set encoding and Locale
;;
(set-language-environment   'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system       'utf-8)

;; language set
(setq current-language-environment "English")

;; if system is mac os x
(defconst *is-a-mac* (eq system-type 'darwin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; adding location for required lisp files and libraries to the path      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom-settings.el will store any custom settings made on Emacs        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file
      (expand-file-name "custom-settings.el" (concat (getenv "HOME") "/.emacs.d")))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load custom methods and internal settings for Emacs                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/aqua-methods.el")
;; now load the custom settings from aqua-internals.el
(add-hook 'after-init-hook
          '(lambda ()
             (load "~/.emacs.d/aqua-internals.el")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aqua-package-repos.el will load the package repository settings        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (concat (getenv "HOME") "/.emacs.d/aqua-package-repos.el"))


;;;------------------------------------------------------------------------;;;
;;; byte recompiling everything during bootstrap                           ;;;
;;; a custom function is defined inside my-methods                         ;;;
;;; uncomment below section if needed                                      ;;;
;;;------------------------------------------------------------------------;;;
(byte-recompile-directory
  (expand-file-name (concat (getenv "HOME") "/.emacs.d/packages/elpa/")) 0)
;;;------------------------------------------------------------------------;;;
(setq debug-on-error t)  ;; finalizers (for debugging)


;;;========================================================================;;;
;;;  specify all the require packages and settings to be loaded initially  ;;;
;;;========================================================================;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diminished modes are minor modes with no modeline display              ;;;
;;; hide a minor mode that you know are always enabled using this          ;;;
;;; http://www.eskimo.com/~seldon/diminish.el                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diminish)
(eval-after-load "whitespace" '(diminish 'whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; identify unnecessary whitespace is in all programming modes            ;;;
;;; whitespace-cleanup command for clearing trailing white spaces          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-default 'indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit (git integration - now using git-gutter)                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (require 'magit)
; (eval-after-load 'magit
;   (progn '(global-set-key (kbd "C-x g") 'magit-status)))
; (define-key global-map (kbd "C-c m") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-easy (helpers for easily building Emacs flymake checkers)      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-easy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show flymake errors in mini buffer                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer mode                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Map pairs of simultaneously pressed keys to commands                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
(key-chord-define-global "fm" 'helm-mini)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit multiple regions simultaneously in a buffer or a region           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set SHELL and pull PATH variables from the .zshrc                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set shell
(setenv "SHELL" "/bin/zsh")
(defun set-exec-path-from-shellpath ()
  "Get the PATH variables from the .zshrc environment file."
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shellpath))

;; path variables
(dolist (dir '("/sbin"
               "/usr/sbin"
               "/bin"
               "/usr/sbin"
               "/usr/local/bin"))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (add-to-list 'exec-path dir))


;;; custom init settings are all completed

(provide 'init)
;;; init.el ends here
