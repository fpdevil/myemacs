;;; package --- aqua-package-repos.el package repository information for Emacs
;;; -*- coding: utf-8 -*-
;;;
;;;           ######## ##     ##    ###     ######   ######
;;;           ##       ###   ###   ## ##   ##    ## ##    ##
;;;           ##       #### ####  ##   ##  ##       ##
;;;           ######   ## ### ## ##     ## ##        ######
;;;           ##       ##     ## ######### ##             ##
;;;           ##       ##     ## ##     ## ##    ## ##    ##
;;;           ######## ##     ## ##     ##  ######   ######
;;;
;;; Commentary:
;;;              All the required packages and modules loading by Emacs
;;;
;;; Filename   : aqua-package-repos.el
;;; Description: This file contains all the packages to be laoded and installed
;;;              by Emacs during startup.  Any new package required by the apps
;;;              or any custom settings needs to be specified here so that they
;;;              can be installed and loaded.  By default the package.el access
;;;              to only the default ELPA repository, so added additional repos
;;;
;;;              Use standard Emacs binding M-x and package-refresh-contents to
;;;              reload  the list of packages after for the first time
;;;
;;; Code:
;;; Updated    : 16 Feb 2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; required default standard libraries
(eval-when-compile (require 'cl))
(require 'cl-lib)

;;============================================================================;;
;;;;     Package repositories (gnu, melpa, melpa-stable and marmalade)      ;;;;
;;============================================================================;;
(require 'package)

(setq package-archives
      '(("elpy"         . "https://jorgenschaefer.github.io/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("marmalade"    . "https://marmalade-repo.org/packages/")
        ))

;; if on Emacs 24.4 or newer, if so, use the pinned package feature
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((elpy                  . "elpy")
          (highlight-indentation . "elpy") ;; fixes error in elpy 1.6
          (org                   . "org")
          (jedi                  . "melpa")
          (markdown-mode         . "melpa-stable")
          (smart-mode-line       . "melpa-stable")
          (swiper                . "melpa-stable")
          (web-mode              . "melpa")
          (which-key             . "melpa-stable")
          )))

(setq package-archive-priorities
      '(("org"          . 30)
        ("elpy"         . 30)
        ("melpa"        . 20)
        ("gnu"          . 10)
        ("melpa-stable" . 10)
        ("marmalade"    . 5)
        ))
(setq package-menu-hide-low-priority t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;           initialize all the defined packages              ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (file-exists-p package-user-dir)
  (message "No packages exist yet, refreshing archives.")
  (package-refresh-contents))
(package-initialize)

(setq url-http-attempt-keepalives nil)
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to check if all listed packages are installed. return true when   ;;
;; package is not installed. When Emacs boots, check to make sure all the     ;;
;; packages defined in required-packages are installed. If not ELPA kicks in. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aqua-packages-installed-p ()
  (cl-loop for p in required-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if not all the packages which are listed are installed, check one by one ;;;
;;; and install the missing ones.                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (aqua-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" ">>> Emacs refreshing its package database...")
  (package-refresh-contents)
  (message "%s" ">>> package refresh done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defun aqua-external-pkg-list ()
  "Check all the external packages not installed via aqua.
Gets all installed packages not in the `required-packages'.
Helpful to get rid of unused packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list required-packages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; upgrade all packages and delete obsolete ones                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aqua-package-upgrade ()
  "Upgrade all the listed packages."
  (interactive)
  (save-window-excursion
    (with-temp-buffer
      (package-list-packages)
      (package-menu-mark-upgrades)
      (package-menu-mark-obsolete-for-deletion)
      (package-menu-execute t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for package installation through use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq use-package-verbose t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; loop through the custom lisp under the vendor directory                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-loop for location in custom-load-paths
         do (add-to-list 'load-path
                         (concat (file-name-directory (or load-file-name
                                                          (buffer-file-name)))
                                 "vendor/"
                                 location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; loop through each and load the configured custom packages              ;;;;
;;;; each configuration file has a format of name-config.el                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (mapc 'load (directory-files module-dir nil "^[^#].*el$"))
(add-to-list 'load-path module-dir)             ;; load the modules dir
(message "Loading the aquamacs modules...")

(require 'misc-config)
(require 'dired-config)
(require 'move-text-config)
(require 'utils-config)
(require 'bookmarks-config)
(require 'fiplr-config)
(require 'quick-peek-config)
(require 'themes-config)
(require 'flycheck-config)
(require 'flymake-config)
(require 'fringe-config)
(require 'company-config)
(require 'ac-complete-config)
(require 'flyspell-config)
;; (require 'spell-config)
(require 'undo-tree-config)
(require 'evil-config)
(require 'yasnippets-config)
(require 'helm-settings-config)
(require 'rbow-config)
(require 'rbow-identifiers-config)
(require 'rainbow-delims-config)
(require 'smart-config)
(require 'paredit-config)
(require 'highlight-symbol-config)
(require 'neotree-config)
(require 'popwin-config)
(require 'ecb-config)
(require 'auto-insert-config)
;; (require 'org-config)
;; (require 'plantuml-config)
;; (require 'slides-config)
(require 'multiple-cursors-config)
(require 'gitgutter-config)
(require 'weather-config)
(require 'whichkey-config)
(require 'guidekey-config)
(require 'beacon-config)
(require 'vregex-config)
;; (require 'psgml-config)
;; (require 'xslide-config)
;; (require 'xslt-process-config)
;; (require 'nxml-config)
;; (require 'cpp-config)
(require 'python-config)
(require 'elpy-python-config)
;; (require 'jedi-python-config)
;; (require 'haskell-config)
;; (require 'erlang-config)
;; (require 'elixir-config)
;; (require 'elisp-config)
;; (require 'scala-config)
;; (require 'go-config)
;; (require 'clojure-config)
;; (require 'vim-config)
;; (require 'web-config)
;; (require 'js-config)
;; (require 'coffee-config)
;; (require 'shell-config)
;; (require 'markdown-config)
;; (require 'yaml-config)
;; (require 'ycm-config)
(require 'projectile-config)
(require 'delighted-config)
(require 'tex-config)
(require 'hippie-config)
;; (require 'semantic-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now load personal elisp files if any from personal directory               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p personal-dir)
  (message "Loading personal configuration files in %s..." personal-dir)
  (mapc 'load (directory-files personal-dir 't "^[^#\.].*el$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'aqua-package-repos)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-package-repos.el ends here
