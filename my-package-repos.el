;;
; my-package-repos.el
;;
;; Use M-x package-refresh-contents to reload the list of
;; packages after adding these for the first time
(require 'cl)
(require 'cl-lib)

(require 'package)
; (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;                          ("melpa"     . "http://melpa.milkbox.net/packages/")))

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)


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

;;=====================================================
;;
; packages to be installed
; defvar is the correct way to declare global variables
; setq is supposed to be use just to set variables and
; not create them.
;;
;;=====================================================
(defvar required-packages
  '(;; appearance
    ;; powerline smart mode
    powerline
    ;; fancy vim airline themes
    airline-themes
    ;; color themes pack
    sublime-themes
    ;; dark theme
    darkokai-theme
    ;; package for various icons
    all-the-icons
    ;; monpkai color theme
    ; monokai-theme
    ;; zenburn theme
    zenburn-theme
    ;; material theme
    material-theme
    ;; essential
    buffer-move
    ;; cmopany autocompletion modes
    company
    company-jedi
    ;; distel-completion is needed for company-distel
    distel-completion-lib
    company-distel
    ;; smex
    ;; colorful modes
    rainbow-delimiters
    rainbow-mode
    rainbow-identifiers
    ;; for auto completion
    auto-complete
    ;; IDO mode
    ido
    ;; utilities
    ;; parenthesis management
    smartparens
    ;; language specific
    markdown-mode
    ;; auctex
    ;; virtualenv wrapper for python
    virtualenvwrapper
    ;; python jedi IDE
    jedi
    ;; python elpy
    elpy
    ;; python linter
    python-pylint
    ;; python yapf
    py-yapf
    ;; flycheck
    flycheck
    ;; flycheck errors display in tooltip
    flycheck-pos-tip
    ;; show flycheck/flymake errors by tooltip
    flycheck-tip
    ;; popup
    popup
    ;; flymake on the fly syntax check
    flymake-easy
    ;; flymake handler for syntax-checking Python source code using pyflakes or flake8
    flymake-python-pyflakes
    ;; linting for haskell
    flymake-hlint
    ;; org-mode
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
    ;; haskell-indentation, 2nd try
    hi2
    ghc
    ;; complete dev environment for haskell
    ;intero
    ;; erlang ide
    erlang
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


;;
; Add the above to the load-path
;;
(unless (file-exists-p save-dir)
  (make-directory save-dir))
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path module-dir)

;;
; Requires packages in the modules/ directory
;;
(mapc 'load (directory-files module-dir nil "^[^#].*el$"))



;;
; method to check if all packages are installed
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


;;
; package loading from custom el files
;;
(defvar packages
    '("haskell-config"
      "erlang-config")
    "configuration files which follow the modules/pkg-name.el format"
    )

;;
; loop through and load the custom packages
;;
(loop for name in packages
      do (load (concat (file-name-directory load-file-name)
                       "modules/"
                       name ".el")))


;;
; packages from custom path
;;
(defvar custom-load-paths
  '("structured-haskell-mode/elisp")
  "Custom load paths that don't follow the normal
  package-name/module-name.el format.")


;;
; loop through the custom lisp
;;
(loop for location in custom-load-paths
      do (add-to-list 'load-path
             (concat (file-name-directory (or load-file-name
                                              (buffer-file-name)))
                     "packages/"
                     location)))