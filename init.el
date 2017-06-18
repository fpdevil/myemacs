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
;;; URL       : https://github.com/fpdevil/myemacs
;;; Commentary:
;;;
;;; filename   : init.el
;;; description: initialization file for loading the necessary packages
;;;
;;; Code:
;;        This sets up the load path so that we can override it
;;        Update Note    : 18 June 2017
;;;=============================================================================

;; utf-8 character set encoding and Locale
(set-language-environment   'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system       'utf-8)

;; language setup
(setq current-language-environment "English")

;; a variable for holding if system is mac os x
(defconst *is-a-mac* (eq system-type 'darwin))
(message "Emacs Version: %d%s%d"
         emacs-major-version
         version-separator
         emacs-minor-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load the newest byte code every time                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; adding location for required lisp files and libraries to the path        ;;;
;; (setq user-emacs-directory (concat (getenv "HOME") "/.emacs.d"))          ;;;
;; (add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d"))             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-emacs-directory  "~/.emacs.d")
(add-to-list 'load-path user-emacs-directory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   custom-settings.el will store any custom settings made on Emacs        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file
      (expand-file-name "custom-settings.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------;;
;;; Define custom directories for the packages                               ;;;
;;; packages/elpa will contain the standard packages installed by Emacs      ;;;
;;; modules dir will contain the custom built and lang specific modules      ;;;
;;; vendor dir will contain 3rd party or unavailable packages                ;;;
;;; Define a top-level, vendor and custom files                              ;;;
;;----------------------------------------------------------------------------;;
(defconst home-dir "~")

(defvar emacs-dir (file-name-directory load-file-name)
  "Top level Emacs dir.")
(defvar emacs-dir (file-name-directory (concat (getenv "HOME") "/.emacs.d"))
  "Top level Emacs dir.")
(defvar core-dir (expand-file-name "core" emacs-dir)
  "Contains core components like the package repository information etc.")
(defvar vendor-dir (expand-file-name "vendor" emacs-dir)
  "Packages not yet available in ELPA.")
(defvar module-dir (expand-file-name "modules" emacs-dir)
  "Personal stuff.")
(defvar personal-dir (expand-file-name "personal" emacs-dir)
  "All personal configuration settings like theme names etc.")
(defvar save-dir (expand-file-name "cache" emacs-dir)
  "Common directory for automatically generated save/history/files/etc.")
(defvar pkg-dir (expand-file-name "packages" emacs-dir)
  "Package installation directory for all Emacs packages.")

;;----------------------------------------------------------------------------;;
;;;;;;;;;;          end of custom directory declaration.              ;;;;;;;;;;
;;----------------------------------------------------------------------------;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   load core package, custom methods and internal settings for Aquamacs   ;;;
;;;   aqua-packages.el
;;;   aqua-methods.el
;;;   aqua-internals.el
;;;   aqua-ui.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path core-dir)             ;; load the core dir first
(message "Loading the aquamac's core...")

(require 'aqua-packages)
(require 'aqua-methods)
(require 'aqua-internals)
(require 'aqua-ui)
(require 'aqua-customizations)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   aqua-package-repos.el will load the package repository settings        ;;;
;;;   (load (concat (getenv "HOME") "/.emacs.d/aqua-package-repos.el"))      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq aqua-pkg-repos
      (expand-file-name "aqua-package-repos.el" user-emacs-directory))
(when (file-exists-p aqua-pkg-repos)
  (message "Loading the aquqmacs package installer...")
  (load aqua-pkg-repos))

;; (load (concat user-emacs-directory "/aqua-package-repos.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   byte recompiling everything during bootstrap (comment | un-comment)    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-recompile-init-files ()
  "Recompile all of the startup files."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "/packages") 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finalizers (for debugging and recompiling)                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq debug-on-error t)
;; (setq debug-on-signal t)
(byte-recompile-init-files)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Make Emacs use the $PATH set up by the user's shell                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   set SHELL and pull PATH variables from the .zshrc                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set shell
(setenv "SHELL" "/bin/zsh")
(defun set-exec-path-from-shellpath ()
  "Get the PATH variables from the .zshrc environment file."
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL -c 'echo -n $PATH'"))))
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

 ;; environment
  (setq exec-path-from-shell-arguments '("-l"))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))

;;; custom init settings completed
;;--------------------------------------------------------------------------------

(provide 'init)
;;; init.el ends here
