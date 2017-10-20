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
;;; Updated    : 20 Oct 2017
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
;; (ADDON package manager) for package installation through use-package
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
(message "Loading the Aquamacs modules...")

(require 'misc-config)                          ;; miscellaneous settings
(require 'dired-config)                         ;; for dired*
(require 'move-text-config)                     ;; text management
(require 'utils-config)                         ;; some utilities like window configurations, imenu-list etc.
(require 'bookmarks-config)                     ;; manage bookmarks
(require 'fiplr-config)                         ;; Fuzzy Find in Project Package
(require 'quick-peek-config)                    ;; Quick-peek inline-window library
(require 'themes-config)                        ;; emacs themes & modeline (sml + airline)
(require 'flycheck-config)                      ;; flycheck syntax checking
(require 'flymake-config)                       ;; flymake syntax checking
(require 'fringe-config)                        ;; thin strip down the left and/or right edge
(require 'company-config)                       ;; company auto completion
(require 'ac-complete-config)                   ;; auto completion ac
(require 'flyspell-config)                      ;; spell check
;; (require 'spell-config)
(require 'undo-tree-config)                     ;; manage undo history
(require 'evil-config)                          ;; emacs vim emulation
(require 'yasnippets-config)                    ;; emacs snippets
(require 'semantic-config)                      ;; emacs semantic completion (C/C++)
(require 'helm-settings-config)                 ;; emacs helm fuzzy
(require 'rbow-config)                          ;; rainbow colors for parentheses
(require 'rbow-identifiers-config)              ;; rainbow colors for variables
(require 'rainbow-delims-config)                ;; ranbow colors for brackets
(require 'smart-config)                         ;; smart paretheses
(require 'paredit-config)                       ;; para management
(require 'highlight-symbol-config)              ;; highlight current symbol
(require 'neotree-config)                       ;; neotree for file explorer
(require 'popwin-config)                        ;; text pop up for completion hints
(require 'ecb-config)                           ;; emacs code browser
(require 'auto-insert-config)                   ;; auto insert tags and snippets
(require 'org-config)                           ;; for org mode
(require 'plantuml-config)                      ;; org mode diagrams
(require 'slides-config)                        ;; org mode presentations
(require 'multiple-cursors-config)              ;; manage multiple cursors
(require 'gitgutter-config)                     ;; vcs management
(require 'weather-config)                       ;; get wether details
(require 'whichkey-config)                      ;; get details of key bindings
(require 'guidekey-config)                      ;; get details of all emacs key mappings
(require 'beacon-config)                        ;; visually show where am i
(require 'vregex-config)                        ;; regex support
(require 'psgml-config)                         ;; markup language support
(require 'xslide-config)                        ;; xsl ide settings
(require 'xslt-process-config)                  ;; for xslt syntax and completion
(require 'nxml-config)                          ;; for xml syntax validation
(require 'cpp-config)                           ;; c++ & c programming language support
(require 'python-config)                        ;; python 3.x.x programming core settings
(require 'elpy-python-config)                   ;; python 3.x.x auto completion through ELPY
(require 'jedi-python-config)                   ;; python 3.x.x auto completion through JEDI
(require 'haskell-config)                       ;; Haskell programming language syntax/auto-complete etc
(require 'erlang-config)                        ;; Erlang programming language syntax/auto-complete etc
(require 'elixir-config)                        ;; ELIXIR programming language syntax/auto-complete etc
(require 'elisp-config)                         ;; ELisp programming language syntax/auto-complete etc
(require 'scala-config)                         ;; Scala programming language syntax/auto-complete etc
(require 'go-config)                            ;; Go programming language syntax/auto-complete etc
(require 'clojure-config)                       ;; Clojure programming language syntax/auto-complete etc
(require 'vim-config)                           ;; VIM Script syntax checking and completion
(require 'web-config)                           ;; for html and web markup langugae support
(require 'js-config)                            ;; Java Script syntax checking, linting & Auto Completion
(require 'coffee-config)                        ;; Coffee Script syntax/auto-complete
(require 'shell-config)                         ;; for shell scripting support
(require 'markdown-config)                      ;; for Markdown files *.md
(require 'yaml-config)                          ;; yaml language support
(require 'ycm-config)                           ;; YouCompleteMe Intelligent Completion
(require 'projectile-config)                    ;; project management
(require 'delighted-config)                     ;; diminish minor modes in mode line
(require 'tex-config)                           ;; TeX mode
(require 'hippie-config)                        ;; hippie expansions

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
