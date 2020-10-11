;; package --- Aquamacs initialization file
;; -*- lexical-binding: t; -*-
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
;;        Update Note    : 09 Apr 2018
;;;=============================================================================
;; (advice-add #'package-initialize :after #'update-load-path)

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(eval-when-compile (require 'cl))     ;; for lexical-let

;;----------------------------------------------------------------------------
;;** get the Emacs initialization time
;;----------------------------------------------------------------------------
(lexical-let ((emacs-startup-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed-time
		     (float-time
		      (time-subtract (current-time) emacs-startup-time))))
		(message "** Emacs Initialized in %.3fs **" elapsed-time)))))

;;----------------------------------------------------------------------------
;;** [Pre-Initialization] - gc threshold setting
;;----------------------------------------------------------------------------
;; (setq garbage-collection-messages t) ; for debug
(setq best-gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024))))

;;----------------------------------------------------------------------------
;;** finalizers (for debugging and recompiling)
;;*** uncomment while debuging
;;----------------------------------------------------------------------------
(setq-default message-log-max t)
(setq debug-on-error t)
;; (setq debug-on-signal t)

;;----------------------------------------------------------------------------
;;** Disable loading of “default.el” at startup
;;----------------------------------------------------------------------------
(setq inhibit-default-init t)

;;----------------------------------------------------------------------------
;;** system and user information (directories | names etc.,)
;;----------------------------------------------------------------------------
(setq user-full-name "Sampath Singamsetty")
(setq user-mail-address "Singamsetty.Sampath@gmail.com")

;;----------------------------------------------------------------------------
;;* Custom group and directory structure
;; Define  user, package  and  custom directories  for  the packages  and
;; modules packages/elpa will contain  the standard packages installed by
;; Emacs  modules dir  will contain  the custom  built and  lang specific
;; modules vendor dir will contain  3rd party or unavailable packages and
;; also Define a top-level, vendor and custom files
;;----------------------------------------------------------------------------

;;** Group Declarations
;;** define a custom group for .emacs
(defgroup dotemacs nil
  "Customized configuration group for .emacs."
  :group 'local)

;;** set/get the user's home directory
(defconst user-home-directory
 (expand-file-name "~/")
 "Default User Home Directory (~/).")

;;** aquamacs directory (same as the user home)
(defconst aqua-dir "~/.emacs.d" "Directory where Aquamacs is installed.")

;;*** specify the emacs user directory
(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".emacs.d" user-home-directory)))


;;** define variables for holding various directory locations
(defcustom sampath-dir user-emacs-directory
  "Junk."
  :group 'dotemacs)

(defcustom vendor-dir (expand-file-name "vendor" user-emacs-directory)
  "Packages not yet available in ELPA."
  :group 'dotemacs)

(defcustom core-dir (expand-file-name "core" user-emacs-directory)
  "Contains core components, which has Emacs/Aquamacs specific information like the packages list, settings etc."
  :group 'dotemacs)

(defcustom module-dir (expand-file-name "modules" user-emacs-directory)
  "All the customizations are placed here."
  :group 'dotemacs)

(defcustom personal-dir (expand-file-name "personal" user-emacs-directory)
  "All personal configuration settings like theme names etc."
  :group 'dotemacs)

(defcustom cache-dir (expand-file-name "cache" user-emacs-directory)
  "Common directory for automatically generated save/history/files/etc."
  :group 'dotemacs)

(defcustom pkg-dir (expand-file-name "elpa" user-emacs-directory)
  "Base package installation directory for all Emacs packages.
The elpa directory will be the one which houses all the .el packages."
  :group 'dotemacs)

(defcustom preferences-dir (expand-file-name "preferences" user-emacs-directory)
  "Custom preferences storage."
  :group 'dotemacs)

(defcustom emacs-major-version 28
  "My custom variable to mask."
  :group 'dotemacs)

;;----------------------------------------------------------------------------
;;** specify to which directory elpa packages are installed in,
;;   default is described in the package.el.
;;   (setq package-user-dir (expand-file-name "elpa" aqua-dir))
;;----------------------------------------------------------------------------
(setq package-user-dir pkg-dir)

;;----------------------------------------------------------------------------
;;** specify which navigation engine to use
;;----------------------------------------------------------------------------
(defcustom dotemacs-switch-engine
  'ivy
  "The primary engine to use for narrowing and navigation."
  :type '(radio
	  (const :tag "helm" helm)
	  (const :tag "ido" ido)
	  (const :tag "ivy" ivy))
  :group 'dotemacs)

;;----------------------------------------------------------------------------
;;** specify which auto completion engine to use
;;----------------------------------------------------------------------------
(defcustom dotemacs-completion-engine
  'company
  "The default Auto Completion engine to be used for all prog modes."
  :type '(radio
	  (const :tag "company-mode" company)
	  (const :tag "auto-complete-mode" auto-complete))
  :group 'dotemacs)

;; Enable/disable yasnippet for company: t or nil
;;(setq dotemacs-company-enable-yas t)

;;----------------------------------------------------------------------------
;;** specify which mode line to use
;;----------------------------------------------------------------------------
(defcustom dotemacs-mode-line
  'sml
  "The default mode line display to be used for all prog modes."
  :type '(radio
	  (const :tag "airline mode line" airline)
	  (const :tag "spaceline" spaceline)
	  (const :tag "sml mode line" sml))
  :group 'dotemacs-visual)

;;----------------------------------------------------------------------------
;;** specify which color identifiers mode to use
;;----------------------------------------------------------------------------
(defcustom dotemacs-clr-identifiers
  'rainbow-identifiers
  "The default color identifiers mode display to be used for all prog modes."
  :type '(radio
	  (const :tag "rainbow-identifiers-mode" rainbow-identifiers)
	  (const :tag "color-identifiers-mode" color-identifiers))
  :group 'dotemacs)

;;----------------------------------------------------------------------------
;;** core settings configuration
;;** load the core .el files
;;----------------------------------------------------------------------------
;;**  load core package, custom methods and internal settings for Aquamacs
;; (let ((file-name-handler-alist nil))
;;   (load-core-config 'aqua-internals)
;;   (load-core-config 'aqua-ui)
;;   (load-core-config 'aqua-methods)
;;   (load-core-config 'aqua-customizations)
;;   (load-core-config 'aqua-packages)
;;   (load-core-config 'aqua-packages-init)
;;   (load-core-config 'aqua-init-benchmark))

(defun load-core (path)
  "Load all elisp files (*.el) of a directory from the supplied argument PATH.
Usage: (load-dir <path>)
Example: (load-dir \"~/.emacs.d/custom\")"
  (mapc (lambda (x) (message x)) (directory-files-recursively core-dir "\\.el$"))
  (mapc #'load (directory-files path t "\\.el$")))

;;**  load core package, custom methods and internal settings for Aquamacs
(load-core core-dir)

;;----------------------------------------------------------------------------
;;** Load all the individual configurations from the modules dir
;;   aqua-package-repos.el will load the package repository settings
;;----------------------------------------------------------------------------
(setq aqua-pkg-repos (expand-file-name "aqua-package-repos.el" user-emacs-directory))
(when (file-exists-p aqua-pkg-repos)
  (message "Loading custom Emacs module configuration from %s" aqua-pkg-repos)
  (load aqua-pkg-repos))

;;----------------------------------------------------------------------------
;;** byte recompiling everything during bootstrap (comment | un-comment)
;----------------------------------------------------------------------------
(defun byte-recompile-init-files ()
  "Recompile all of the startup files."
  (interactive)
  (byte-recompile-directory pkg-dir 0))

;;(byte-recompile-init-files)
;;(setq load-prefer-newer t) ;; load the newest byte code every time

;;----------------------------------------------------------------------------
;;** customizations.el will store any custom settings made on Emacs
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(load custom-file :noerror :nomessage)

;;----------------------------------------------------------------------------
;;** Emacs environment variables loading
;;***  set SHELL and pull PATH variables from the .zshrc (first set the shell)
;;----------------------------------------------------------------------------
(setenv "SHELL" "/bin/zsh")

;; set PATH correctly from the shell using a utility function to help find
;; any executable. may be required for the OS X environments
(defun aqua/set-env-from-shell (env-var)
  "Set up Emacs' `exec-path' using ENV-VAR to match one used by user's shell.
This might be particularly useful for Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((env-var-from-shell (replace-regexp-in-string
			     "[ \t\n]*$" ""
			     (shell-command-to-string
			      (format "$SHELL --login -i -c 'echo $%s'" env-var)))))
    (setenv env-var env-var-from-shell)))

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


;;**
;;*** for Mac OSX...
;; (if window-system (set-exec-path-from-shellpath))
;; (if (equal window-system 'ns)
;;     (push "/Applications/Emacs.app/Contents/MacOS/bin" exec-path))

;;** Haskell Environment loading
;;*** for haskell ghc-mod
;;    path setting needed for identifying the ghc-mod package
;;    for some reason the same specified in haskell custom configuration
;;    is not working, so placing it here
;; (let ((my-cabal-path (expand-file-name (concat (getenv "HOME") "/.cabal/bin"))))
;;   ;; setup the cabal path and put into classpath
;;   (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:

;;; init.el ends here
