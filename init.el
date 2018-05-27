;; package --- Aquamacs initialization file
;; -*- lexical-binding: t; -*-
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
;;        Update Note    : 09 Apr 2018
;;;=============================================================================
(eval-when-compile (require 'cl))

;;** get the Emacs initialization time
(lexical-let ((emacs-startup-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed-time (float-time (time-subtract (current-time) emacs-startup-time))))
                (message "$$ Emacs Initialized in %.3fs $$" elapsed-time)))))


;;;** gc threshold setting
;; (setq garbage-collection-messages t) ; for debugging prupose
(setq gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)


;;** define a custom group for .emacs
(defgroup dotemacs nil
  "Customized configuration for .emacs."
  :group 'local)

;;** load the newest byte code every time
;; (byte-recompile-init-files)
(setq load-prefer-newer t)

;;** finalizers (for debugging and recompiling)
;; (setq debug-on-error t)
;; (setq debug-on-signal t)
(setq-default message-log-max 10000)

;;** Disable loading of “default.el” at startup,
(setq inhibit-default-init t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define user, package and custom directories for the packages and modules   ;;
;; packages/elpa will contain the standard packages installed by Emacs        ;;
;; modules dir will contain the custom built and lang specific modules        ;;
;; vendor dir will contain 3rd party or unavailable packages                  ;;
;; Define a top-level, vendor and custom files                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst aqua-dir "~/.emacs.d" "Directory where Aquamacs is installed.")

;;*** specify the emacs user directory
(setq user-emacs-directory aqua-dir)

;;*** specify to which directory elpa packages are installed in, Defined in package.el.
(setq package-user-dir (expand-file-name "elpa" aqua-dir))

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

(defcustom pkg-dir (expand-file-name "elpa" user-emacs-directory)
  "Base package installation directory for all Emacs packages.
The elpa directory will be the one which houses all the .el packages."
  :group 'dotemacs)


;;** specify which navigation engine to use
(defcustom dotemacs-switch-engine
  'helm
  "The primary engine to use for narrowing and navigation."
  :type '(radio
          (const :tag "helm" helm)
          (const :tag "ido" ido)
          (const :tag "ivy" ivy))
  :group 'dotemacs)

;;** specify which auto completion engine to use
(defcustom dotemacs-completion-engine
  'company
  "The default Auto Completion engine to be used for all prog modes."
  :type '(radio
          (const :tag "company-mode" company)
          (const :tag "auto-complete-mode" auto-complete))
  :group 'dotemacs)

;;** specify which mode line to use
(defcustom dotemacs-mode-line
  'spaceline
  "The default mode line display to be used for all prog modes."
  :type '(radio
          (const :tag "airline mode line" airline)
          (const :tag "spaceline" spaceline)
          (const :tag "sml mode line" sml))
  :group 'dotemacs-visual)

;;** specify which color identifiers mode to use
(defcustom dotemacs-clr-identifiers
  'color-identifiers
  "The default color identifiers mode display to be used for all prog modes."
  :type '(radio
          (const :tag "rainbow-identifiers-mode" rbow-identifiers)
          (const :tag "color-identifiers-mode" color-identifiers))
  :group 'dotemacs)


;;** load the core .el files
(defmacro load-core-config (pkg)
  `(load (file-truename (format (concat core-dir "/%s") ,pkg))))

;;**  load core package, custom methods and internal settings for Aquamacs
;;   == aqua-packages.el
;;   == aqua-packages-init.el
;;   == aqua-methods.el
;;   == aqua-internals.el
;;   == aqua-ui.el
(let ((file-name-handler-alist nil))
  (load-core-config 'aqua-init-benchmark)
  (load-core-config 'aqua-internals)
  (load-core-config 'aqua-packages)
  (load-core-config 'aqua-packages-init)
  (load-core-config 'aqua-methods)
  (load-core-config 'aqua-ui)
  (load-core-config 'aqua-customizations))

;;**  aqua-package-repos.el will load the package repository settings
;;(setq aqua-pkg-repos
;;      (expand-file-name "aqua-package-repos-minimal.el" user-emacs-directory))
(setq aqua-pkg-repos
      (expand-file-name "aqua-package-repos.el" user-emacs-directory))
(when (file-exists-p aqua-pkg-repos)
  (message "Loading the custom Emacs modules...")
  (load aqua-pkg-repos))

;;**  byte recompiling everything during bootstrap (comment | un-comment)
(defun byte-recompile-init-files ()
  "Recompile all of the startup files."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "/packages") 0))

;;**  custom.el will store any custom settings made on Emacs
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;; (load custom-file :no-error :no-message)
(when (file-exists-p custom-file) (load custom-file 'noerror))

;;**  set SHELL and pull PATH variables from the .zshrc
;;   == first set the shell
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

;; == system path variables
(dolist (dir '("/sbin"
               "/usr/sbin"
               "/bin"
               "/usr/sbin"
               "/usr/local/bin"))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (add-to-list 'exec-path dir))

;; == environment
(setq exec-path-from-shell-arguments '("-l"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; == for haskell ghc-mod
;; path setting needed for identifying the ghc-mod package
;; for some reason the same specified in haskell custom configuration
;; is not working, so placing it here
(let ((my-cabal-path (expand-file-name (concat (getenv "HOME") "/Library/Haskell/bin"))))
  ;; setup the cabal path and put into classpath
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))


;;;;;;;;;;;;;;;;;;;;;;; custom init settings completed ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; init.el ends here
