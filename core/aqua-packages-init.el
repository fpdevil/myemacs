;;; package --- package repositories and functions for installing packages
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-packages-init.el
;;; description: This file contains all the package repositories, pinned package
;;;              information,  package priorities and  the automation functions
;;;              required for installing all the listed and required packages.
;;;
;;; Code:
;;; Updated    : 06 Apr 2018
;;;=============================================================================
;;**
;; required default standard libraries
(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'package)

;;----------------------------------------------------------------------------
;; Package repositories (gnu, melpa, melpa-stable and marmalade)
;;----------------------------------------------------------------------------
(setq package-archives
      '(
        ;; uncomment/comment the below line for GNU ELPA
        ("gnu"            . "https://elpa.gnu.org/packages/")
        ("melpa"          . "http://melpa.org/packages/")
        ("melpa-stable"   . "http://stable.melpa.org/packages/")
        ("melpa-unstable" . "http://stable.melpa.org/packages/")
        ("elpy"           . "https://jorgenschaefer.github.io/packages/")
        ("org"            . "http://orgmode.org/elpa/")
        ;;("marmalade"      . "http://marmalade-repo.org/packages/")
        ))


(setq package-check-signature nil)
(setq tls-program
      ;; Defaults:
      '("gnutls-cli --insecure -p %p %h"
        "gnutls-cli --insecure -p %p %h --protocols ssl3"
        "openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

;;**
;; if on Emacs 24.4 or newer, if so, use the pinned package feature
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '(
          (elpy                  . "melpa-stable")
          (highlight-indentation . "elpy") ;; fixes error in elpy 1.6
          (org                   . "org")
          (org-plus-contrib      . "org")
          (org-download          . "org")
          (jedi                  . "melpa-stable")
          (jedi-core             . "melpa-stable")
          (company-jedi          . "melpa-stable")
          (markdown-mode         . "melpa")
          (smart-mode-line       . "melpa")
          (ensime                . "melpa-stable")
          (clj-refactor          . "melpa-unstable") ;; note from http://planet.clojure.in/
          (monroe                . "melpa-stable")
          (smart-mode-line       . "melpa-stable")
          (web-mode              . "melpa")
          (counsel               . "melpa")
          (which-key             . "melpa"))))


(setq package-menu-hide-low-priority t)

;;----------------------------------------------------------------------------
;;** set it to `t' in order to use a safer HTTPS to download packages
;;----------------------------------------------------------------------------
(defvar melpa-use-https-repo nil
  "By default, HTTP is used to download packages.
But you may use safer HTTPS instead.")

;;----------------------------------------------------------------------------
;;** initialize all the defined packages
;;----------------------------------------------------------------------------
(unless (file-exists-p package-user-dir)
  (message "No packages exist yet, refreshing archives.")
  (package-refresh-contents))

(setq package-enable-at-startup nil)
(package-initialize)

;;----------------------------------------------------------------------------
;;** define a default package installation function
;;----------------------------------------------------------------------------
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;----------------------------------------------------------------------------
;;** define a function for checking the package loading
;;----------------------------------------------------------------------------
(defmacro after (feature &rest body)
  "Execute FEATURE and REST in BODY after loading,.
FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))."
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))


;;----------------------------------------------------------------------------
;;** for benchmarking
;;----------------------------------------------------------------------------
(require-package 'benchmark-init)
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
;; (add-hook 'after-init-hook 'benchmark-init/activate)

;;----------------------------------------------------------------------------
;;** pretty highlighting for the require-package
;;----------------------------------------------------------------------------
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(require-package\\)\\>" 1 font-lock-builtin-face)))


;;----------------------------------------------------------------------------
;;**  function to check if all listed packages are installed. return true when
;;**  package is not installed. When Emacs boots, check to make sure all the
;;**  packages defined in required-packages are installed. If not ELPA kicks in
;;----------------------------------------------------------------------------
(defun aqua-packages-installed-p ()
  "Check if packages are installed or not."
  (cl-loop for p in required-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

;;----------------------------------------------------------------------------
;;** [use-package] - AddOn package manager for package installation
;;----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t
      use-package-always-defer t
      use-package-always-ensure t
      use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;;----------------------------------------------------------------------------
;;**   if not all the packages which are listed are installed,
;;**   check one by one and install the missing ones.
;;----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;;** upgrade all packages and delete obsolete ones
;;----------------------------------------------------------------------------
(defun aqua-package-upgrade ()
  "Upgrade all the listed packages."
  (interactive)
  (save-window-excursion
    (with-temp-buffer
      (package-list-packages)
      (package-menu-mark-upgrades)
      (package-menu-mark-obsolete-for-deletion)
      (package-menu-execute t))))

;;----------------------------------------------------------------------------
;;** loop through the custom lisp under the vendor directory
;;** load all the .el files from the vendor package
;;----------------------------------------------------------------------------
(cl-loop for location in custom-load-paths
         do (add-to-list 'load-path
                         (message "loading vendor pkg %s" location)
                         (concat
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory
                             (or load-file-name (buffer-file-name)))))
                          "vendor/"
                          location)))

; (cl-loop for location in custom-load-paths
;          do (add-to-list 'load-path
;                          (concat (file-name-directory (or load-file-name
;                                                           (buffer-file-name)))
;                                  "vendor/"
;                                  location)))

;;----------------------------------------------------------------------------
;;** Standard file extensions for which appropriate packages would be
;;** installed automatically if not already present
;;----------------------------------------------------------------------------
(defmacro autoload-lazy-major-mode (pattern mode)
  "Defines a new `major-mode' matched by PATTERN, and install the MODE if necessary, and activates the same."
  `(add-to-list 'auto-mode-alist
                '(,pattern . (lambda ()
                               (require-package (quote ,mode))
                               (,mode)))))

;;**
;;** auto modes for which the packages will be installed
(autoload-lazy-major-mode "CMakeLists\\.txt'" cmake-mode)
(autoload-lazy-major-mode "PKGBUILD\\'" pkgbuild-mode)
(autoload-lazy-major-mode "\\.vim\\(rc\\)?\\'" vimrc-mode)
(autoload-lazy-major-mode "\\.csv$" csv-mode)
(autoload-lazy-major-mode "\\.elm$\\'" elm-mode)
(autoload-lazy-major-mode "\\.groovy$\\'" groovy-mode)
(autoload-lazy-major-mode "\\.lua$\\'" lua-mode)
(autoload-lazy-major-mode "\\.cmake$\\'" cmake-mode)
(autoload-lazy-major-mode "\\.php$\\'" php-mode)
(autoload-lazy-major-mode "\\.proto$\\'" protobuf-mode)
(autoload-lazy-major-mode "\\.rs$\\'" rust-mode)
(autoload-lazy-major-mode "\\.swift$\\'" swift-mode)
(autoload-lazy-major-mode "\\.coffee$\\'" coffee-mode)
(autoload-lazy-major-mode "\\.\\(yml\\|yaml\\)$" yaml-mode)
(autoload-lazy-major-mode "Dockerfile\\'" dockerfile-mode)

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-packages-init)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; aqua-packages-init.el ends here
