;; package --- Aquamacs initialization file
;; -*- coding: utf-8 -*-
;; -*-no-byte-compile: t; -*-
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
;;;
;;;
;;; The MIT License:
;;
;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Commentary:
;;;
;;; filename   : init.el
;;; description: initialization file for loading the necessary packages
;;;
;;; Code:
;;        This sets up the load path so that we can override it
;;        Update Note    : 13 Nov 2017
;;;=============================================================================
(eval-when-compile (require 'cl))

;;; -- get the Emacs initialization time
(lexical-let ((emacs-startup-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed-time (float-time (time-subtract (current-time) emacs-startup-time))))
                (message "$$ Emacs Initialized in %.3fs $$" elapsed-time)))))


;;; -- gc threshold setting
(setq gc-cons-threshold (* 256 1024 1024))
(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)))

;;; -- define a custom group for .emacs
(defgroup dotemacs nil
 "Customized configuration for .emacs."
 :group 'local)

;;; -- load the newest byte code every time
(setq load-prefer-newer t)
;; (byte-recompile-init-files)

;;; -- finalizers (for debugging and recompiling)
;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq-default message-log-max 1000)

;;; -- adding location for required lisp files and libraries to the path
(setq user-emacs-directory  "~/.emacs.d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define custom directories for the packages                               ;;;
;;; packages/elpa will contain the standard packages installed by Emacs      ;;;
;;; modules dir will contain the custom built and lang specific modules      ;;;
;;; vendor dir will contain 3rd party or unavailable packages                ;;;
;;; Define a top-level, vendor and custom files                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst home-dir "~")

; (defvar emacs-dir (file-name-directory load-file-name)
;   "Top level Emacs dir.")

; (defvar emacs-dir (file-name-directory (concat (getenv "HOME") "/.emacs.d"))
;   "Top level Emacs dir.")

(defcustom core-dir (expand-file-name "core" user-emacs-directory)
  "Contains core components, which has Emacs/Aquamacs specific information like the packages list, settings etc."
  :group 'dotemacs)

(defcustom vendor-dir (expand-file-name "vendor" user-emacs-directory)
  "Packages not yet available in ELPA."
  :group 'dotemacs)

(defcustom module-dir (expand-file-name "modules" user-emacs-directory)
  "Personal stuff."
  :group 'dotemacs)

(defcustom personal-dir (expand-file-name "personal" user-emacs-directory)
  "All personal configuration settings like theme names etc."
  :group 'dotemacs)

(defcustom cache-dir (expand-file-name "cache" user-emacs-directory)
  "Common directory for automatically generated save/history/files/etc."
  :group 'dotemacs)

(defcustom pkg-dir (expand-file-name "packages" user-emacs-directory)
  "Base package installation directory for all Emacs packages.
Under this the elpa directory will be present which houses all the .el packages."
  :group 'dotemacs)

(defconst package-user-dir (expand-file-name "elpa" pkg-dir)
  "Which directory elpa packages is installed in. Defined in package.el.")

;;; -- specify which auto completion engine to use
(defcustom dotemacs-completion-engine
 'company
 "The default Auto Completion engine to be used for all prog modes."
 :type '(radio
         (const :tag "company-mode" company)
         (const :tag "auto-complete-mode" auto-complete))
 :group 'dotemacs)

;;; -- specify which mode line to use
(defcustom dotemacs-mode-line
 'spaceline
 "The default mode line display to be used for all prog modes."
 :type '(radio
         (const :tag "airline mode line" airline)
         (const :tag "spaceline" spaceline)
         (const :tag "sml mode line" sml))
 :group 'dotemacs-visual)

;;; -- specify which color identifiers mode to use
(defcustom dotemacs-clr-identifiers
 'color-identifiers
 "The default color identifiers mode display to be used for all prog modes."
 :type '(radio
         (const :tag "rainbow-identifiers-mode" rbow-identifiers)
         (const :tag "color-identifiers-mode" color-identifiers))
 :group 'dotemacs)

;;; --  load core package, custom methods and internal settings for Aquamacs
;;;   == aqua-packages.el
;;;   == aqua-packages-init.el
;;;   == aqua-methods.el
;;;   == aqua-internals.el
;;;   == aqua-ui.el
;;; (add-to-list 'load-path core-dir)             ;; load the core dir first
(require 'aqua-internals (concat core-dir "/aqua-internals.el"))
(require 'aqua-packages (concat core-dir "/aqua-packages.el"))
(require 'aqua-packages-init (concat core-dir "/aqua-packages-init.el"))
(require 'aqua-methods (concat core-dir "/aqua-methods.el"))
(require 'aqua-ui (concat core-dir "/aqua-ui.el"))
(require 'aqua-customizations (concat core-dir "/aqua-customizations.el"))

;;; --  aqua-package-repos.el will load the package repository settings      ;
(setq aqua-pkg-repos
      (expand-file-name "aqua-package-repos.el" user-emacs-directory))
(when (file-exists-p aqua-pkg-repos)
  (message "Loading the aquqmacs packages from the repo...")
  (load aqua-pkg-repos))

;;; --  byte recompiling everything during bootstrap (comment | un-comment)
(defun byte-recompile-init-files ()
  "Recompile all of the startup files."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "/packages") 0))

;;; --  custom.el will store any custom settings made on Emacs
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;; --  set SHELL and pull PATH variables from the .zshrc
;;;   == first set the shell
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

;;; == system path variables
(dolist (dir '("/sbin"
               "/usr/sbin"
               "/bin"
               "/usr/sbin"
               "/usr/local/bin"))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (add-to-list 'exec-path dir))

;;; == environment
(setq exec-path-from-shell-arguments '("-l"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; path setting needed for identifying the ghc-mod package
(let ((my-cabal-path (expand-file-name (concat (getenv "HOME") "/Library/Haskell/bin"))))
  ; setup the cabal path and put into classpath
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (message "**** cabal-path %s ****" my-cabal-path)
  (add-to-list 'exec-path my-cabal-path))


;;;;;;;;;;;;;;;;;;;;;;; custom init settings completed ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; init.el ends here
