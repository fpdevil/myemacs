;-------------------------------------------------------------------------------
;;
; my-package-repos.el
;;
;-------------------------------------------------------------------------------
;; Use M-x package-refresh-contents to reload the list of
;; packages after adding these for the first time
;; required standard libraries
(require 'cl)
(require 'cl-lib)

(require 'package)
; (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;                          ("melpa"     . "http://melpa.milkbox.net/packages/")))

;===============================================================================
; Package repositories
;===============================================================================
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;; package archive priorities
; (setq package-archive-priorities
;       '(("marmalade" . 20)
;         ("gnu" . 10)
;         ("melpa" . 0)
;         ("melpa-stable" . 0)))


;; Define custom directories for the packages
;; packages/elpa will contain the standard packages
;; modules dir will contain the custom built and lang specific modules
;; vendor dir will contain 3rd party or unavailable packages
;; Define a top-level, vendor and custom files
(defvar emacs-dir (file-name-directory load-file-name)
  "Top level Emacs dir.")
(defvar emacs-dir (file-name-directory "~/.emacs.d")
  "Top level Emacs dir.")
(defvar vendor-dir (expand-file-name "vendor" emacs-dir)
  "Packages not yet available in ELPA.")
(defvar module-dir (expand-file-name "modules" emacs-dir)
  "Personal stuff.")
(defvar save-dir (expand-file-name "cache" emacs-dir)
  "Common directory for automatically generated save/history/etc. files.")
(defvar pkg-dir (expand-file-name "packages" emacs-dir)
    "Package installation directory for all emacs packages.")

(add-to-list 'load-path pkg-dir)
(setq package-user-dir (concat pkg-dir "/elpa"))


;;
; initialize the packages
;;
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;===============================================================================
;;
; packages to be installed
; defvar is the correct way to declare global variables
; setq is supposed to be use just to set variables and
; not create them.
;;
;===============================================================================
(defvar required-packages
  '(;; appearance
    ;; powerline smart mode
    powerline
    ;; colorful modes (delimiters and color codes)
    rainbow-delimiters
    rainbow-mode
    rainbow-identifiers
    ;; fancy vim airline themes
    airline-themes
    ;; color themes pack
    sublime-themes
    darkokai-theme
    moe-theme
    monokai-theme
    zenburn-theme
    material-theme
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized
    ;; package for various icons
    all-the-icons
    ;; essential packs
    buffer-move
    ;; auto completions
    ;; cmopany autocompletion modes
    company
    company-jedi
    company-distel
    ;; distel-completion is needed for company-distel
    distel-completion-lib
    ;; auto completion for gnu emacs
    auto-complete
    auto-complete-distel
    ;; backend that emulates ac-source-dictionary
    company-dict
    ;; get major mode's parent modes
    parent-mode
    ;; IDO mode
    ido
    ; smex
    ;; utilities
    ;; parenthesis management
    smartparens
    ;; minor mode for editing parentheses
    paredit
    ;; language specific
    markdown-mode
    ; auctex
    ;; virtualenv wrapper for python
    virtualenvwrapper
    ;; python jedi IDE
    jedi
    ;; python elpy IDE
    elpy
    ;; python linter
    python-pylint
    ;; python yapf
    py-yapf
    ;; python virtual environment interface for Emacs
    pyvenv
    ;; flycheck
    flycheck
    ;; flycheck colors for highlighting errors
    flycheck-color-mode-line
    ;; flycheck errors display in tooltip
    flycheck-pos-tip
    ;; show flycheck/flymake errors by tooltip
    flycheck-tip
    ;; show popup for flycheck
    popup
    ;; flymake on the fly syntax checker
    flymake-easy
    ;; flymake handler for syntax-checking Python source code using pyflakes or flake8
    flymake-python-pyflakes
    ; linting for haskell
    flymake-hlint
    ;; show flymake errors in minibuffer
    flymake-cursor
    ;; org-mode setup
    org
    org-bullets
    ;; Yasnippets package
    yasnippet
    ;; git integration
    magit
    ;; Diminished modes are minor modes with no modeline display
    diminish
    ;; emacs code browser
    ecb
    ;; haskell programming mode
    haskell-mode
    company-ghc
    company-cabal
    shm
    haskell-snippets
    hindent
    flycheck-haskell
    ;; for haskell-indentation, 2nd try
    hi2
    ghc
    ;; complete dev environment for haskell
    ; intero
    ;; erlang laguage support
    erlang
    ; edts
    ;; scala edevelopment with ensime
    ensime
    ;; incremental completion and selection narrowing framework
    helm
    ;; map pairs of simultaneously pressed keys to commands
    key-chord
    ;; math input symbols
    xah-math-input
  )
  "A list of packages that will be installed if not present when firing Emacs")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; Add the above packages to the load-path
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (file-exists-p save-dir)
  (make-directory save-dir))
(add-to-list 'load-path module-dir)
(add-to-list 'load-path vendor-dir)


;;
; Requires packages in the modules/ directory
(mapc 'load (directory-files module-dir nil "^[^#].*el$"))
; Requires packages in the vendor/ directory
(mapc 'load (directory-files vendor-dir nil "^[^#].*el$"))



;;
; function to check if all listed packages are installed
; return true when the package is not installed
;;
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
            finally (return t)))

;;
; if not all packages are installed, check one by one and install the missing ones.
;;
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'required-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; package loading from custom el files
; currently support for the below
; haskell
; erlang
; python3
; flycheck
; paredit
; rainbow-delimiters
;
;;
(defvar configs
    '(
      "haskell-config"
      "erlang-config"
      "python-config"
      "flycheck-colors-config"
      "rainbow-delims-config"
      "paredit-config"
      )
    "configuration files which follow the modules/pkgname-config.el format"
    )

;;
; loop through and load the configured custom packages
;;
(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "modules/"
                       name ".el")))


;;
; load packages from custom path
;;
(defvar custom-load-paths
  '("erlang/elisp")
  "custom load paths that do not follow the normal vendor/elisp/module-name.el format"
  )

;;
; loop through the custom lisp
;;
(loop for location in custom-load-paths
      do (add-to-list 'load-path
             (concat (file-name-directory (or load-file-name
                                              (buffer-file-name)))
                     "vendor/erlang/elisp"
                     location)))
