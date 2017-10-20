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
(require 'package)

;;============================================================================;;
;;;;     Package repositories (gnu, melpa, melpa-stable and marmalade)      ;;;;
;;============================================================================;;
; (add-to-list 'package-archives '("gnu"          . "http://elpa.gnu.org/packages/"))
; (add-to-list 'package-archives '("melpa"        . "http://melpa.milkbox.net/packages/") t)
; (add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/"))
; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
; (add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/"))

(setq package-archives
      '(
        ("elpy"      . "https://jorgenschaefer.github.io/packages/")
        ("gnu"       . "https://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; in case if package archive priorities needs to be specified
; the below section may be used; for now just commented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq package-archive-priorities
;       '(("melpa-stable" . 20)
;         ("gnu"          . 15)
;         ("melpa"        . 10)
;         ("marmalade"    . 5)
;         ("org"          . 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;           initialize all the defined packages              ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (file-exists-p package-user-dir)
  (message "No packages exists yet, refreshing archives.")
  (package-refresh-contents))
(package-initialize)
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursively add sub-folders in a folder to path                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (add-subfolders-to-load-path name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Load the requires packages in the vendor                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(mapc 'load (directory-files vendor-dir nil "^[^#].*el$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to check if all listed packages are installed. return true when   ;;
;; package is not installed. When Emacs boots, check to make sure all the     ;;
;; packages defined in required-packages are installed. If not ELPA kicks in. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aqua-packages-installed-p ()
  "Check if all the packages listed in `required-packages' are installed."
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun aqua-require-pkg (pkg)
  "Check and install each PKG unless its already there."
  (unless (memq pkg required-packages)
    (add-to-list 'required-packages pkg))
  (unless (package-installed-p pkg)
    (message ">>> installing missing package >>> %s" pkg)
    (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if not all the packages which are listed are installed, check one by one ;;;
;;; and install the missing ones.                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aqua-require-packages (pkgs)
  "Check that all PKGS are installed.
Packages missing in the location will be automatically installed."
  (mapc #'aqua-require-pkg pkgs))

(defun aqua-install-packages ()
  "Install each package listed in `required-packages'."
  (unless (aqua-packages-installed-p)
    ;; check for the new packages (package versions)
    (message "%s" " Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " package refresh done!")
    ;; install any missing packages
    (aqua-require-packages required-packages)))

;; now run the package installation process
(aqua-install-packages)


;; (defun aqua-uninstalled-packages (packages)
;;   (delq nil
;;         (mapcar (lambda (p)
;;                   (message ">> checking the package %s" p)
;;                   (if (package-installed-p p nil) nil p))
;;                 packages)))
;;
;; (let ((need-to-install
;;        (aqua-uninstalled-packages required-packages)))
;;   (when need-to-install
;;     (progn
;;       (package-refresh-contents)
;;       (dolist (p need-to-install)
;;         (package-install p)))))


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
;; package loading of  all the custom el files  which contains customized
;; settings  for each  major/minor modes  as well  as any  other packages
;; currently the below are all customized supported configurations.
;;
;; miscellaneous settings
;; some utilities like window configurations, dired*, imenu-list etc.
;; quick-peek
;; code snippets with yas
;; themes
;; flycheck
;; flymake
;; fringe
;; company
;; company-quickhelp
;; auto-complete
;; semanticdb configuration
;; flyspell
;; bookmarks
;; helm
;; smart parentheses
;; parenthesis edit
;; rainbow-mode
;; rainbow identifiers
;; rainbow-delimiters
;; hihlight-symbols
;; neotree
;; popup window
;; Emacs code browser ecb
;; Auto Insert code template headers
;; org mode
;; plantuml for org diagrams
;; org reveal and html5
;; multiple-cursors
;; gitgutter-config
;; weather info
;; which-key and guide-key
;; beacon cursor highlight
;; evil
;; xslt transformations
;; xml using nxml
;; aggressive indentation
;; haskell
;; erlang
;; python3
;; scala
;; elixir
;; elisp
;; go
;; c/c++
;; javascript
;; CoffeeScript
;; Web html etc
;; clojure
;; VIM
;; markdown
;; yaml support
;; shell scripting
;; you complete me
;; projectile
;; delight and dim
;; latex configuration
;; hippie expansion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar configs
  '(
      "misc-config"
      "dired-config"
      "move-text-config"
      "utils-config"
      "bookmarks-config"
      "fiplr-config"
      "quick-peek-config"
      "themes-config"
      "flycheck-config"
      "flymake-config"
      "fringe-config"
      "company-config"
      "ac-complete-config"
      "flyspell-config"
      ;"spell-config"
      "undo-tree-config"
      "evil-config"
      "yasnippets-config"
      "semantic-config"
      "helm-settings-config"
      "rbow-config"
      "rbow-identifiers-config"
      "rainbow-delims-config"
      "smart-config"
      "paredit-config"
      "highlight-symbol-config"
      "neotree-config"
      "popwin-config"
      "ecb-config"
      "auto-insert-config"
      "org-config"
      "plantuml-config"
      "slides-config"
      "multiple-cursors-config"
      "gitgutter-config"
      "weather-config"
      "whichkey-config"
      "guidekey-config"
      "beacon-config"
      "vregex-config"
      "psgml-config"
      "xslide-config"
      "xslt-process-config"
      "nxml-config"
      "cpp-config"
      "elpy-python-config"
      "jedi-python-config"
      "haskell-config"
      "erlang-config"
      "elixir-config"
      "elisp-config"
      "scala-config"
      "go-config"
      "clojure-config"
      "vim-config"
      "web-config"
      "js-config"
      "coffee-config"
      "shell-config"
      "markdown-config"
      "yaml-config"
      "ycm-config"
      "projectile-config"
      "delighted-config"
      "tex-config"
      "hippie-config"
      )
    "Configuration files which follow the modules/pkgname-config.el format.")


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
(cl-loop for name in configs
         do (load (concat (file-name-directory load-file-name)
                          "modules/"
                          name ".el")))

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
