;;; package --- aqua-package-repos.el package respository information for Emacs
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
;;;              to only the default ELPA repository.  So added additional repos
;;;
;;; Updated    : 01 Dec 2016
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; Use M-x package-refresh-contents to reload the list of
;; packages after adding these for the first time
;; required default standard libraries
;
(require 'cl)
(require 'cl-lib)
(require 'package)


;;============================================================================;;
;;;;     Package repositories (gnu, melpa, melpa-stable and marmalade)      ;;;;
;;============================================================================;;
(add-to-list 'package-archives
             '("gnu"          . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa"        . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org"          . "http://orgmode.org/elpa/") t)
;;============================================================================;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; incase if package archive priorities needs to be specified
; the below section may be used; for now just commented
;;
; (setq package-archive-priorities
;       '(("marmalade" . 20)
;         ("gnu" . 10)
;         ("melpa" . 0)
;         ("melpa-stable" . 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define custom directories for the packages                               ;;;
;;; packages/elpa will contain the standard packages installed by Emacs      ;;;
;;; modules dir will contain the custom built and lang specific modules      ;;;
;;; vendor dir will contain 3rd party or unavailable packages                ;;;
;;; Define a top-level, vendor and custom files                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst home-dir "~")
(defvar emacs-dir (file-name-directory load-file-name)
  "Top level Emacs dir.")
(defvar emacs-dir (file-name-directory (concat (getenv "HOME") "/.emacs.d"))
  "Top level Emacs dir.")
(defvar vendor-dir (expand-file-name "vendor" emacs-dir)
  "Packages not yet available in ELPA.")
(defvar module-dir (expand-file-name "modules" emacs-dir)
  "Personal stuff.")
(defvar save-dir (expand-file-name "cache" emacs-dir)
  "Common directory for automatically generated save/history/files/etc.")
(defvar pkg-dir (expand-file-name "packages" emacs-dir)
  "Package installation directory for all Emacs packages.")

(add-to-list 'load-path pkg-dir)
(setq package-user-dir (concat pkg-dir "/elpa"))
;;----------------------------------------------------------------------------;;
;;;;;;;;;;          end of custom directory declaration.              ;;;;;;;;;;
;;----------------------------------------------------------------------------;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;           initialize all the defined packages              ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)
; make sure to have downloaded the archive description.
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install-selected-packages)


;;============================================================================;;
;;;; new or missing packages to be installed during Emacs bootstrap.        ;;;;
;;;; defvar is the correct way to declare global variables                  ;;;;
;;;; setq is supposed to be use just to set variables and not create them.  ;;;;
;;============================================================================;;
(defvar required-packages
  '(;;;;;; appearance and visual customizations ;;;;;;
    powerline                                   ;; powerline smart mode
    ;smart-mode-line                            ;; powerful and beautiful mode-line
    delight                                     ;; customize mode names on modeline
    dim                                         ;; mode-line names of major/minor modes
    rainbow-delimiters                          ;; colorful modes (delimiters and color codes)
    rainbow-mode                                ;; colored identifiers
    rainbow-identifiers                         ;; colored identifiers
    airline-themes                              ;; fancy vim airline themes
    ;;;;;;  color themes for emacs              ;;;;;;
    color-theme                                 ;; install color themes
    sublime-themes                              ;; sublime themes
    darkokai-theme                              ;; dark theme based on monokai
    dracula-theme                               ;; dracula dark theme
    moe-theme                                   ;; group of moe themes
    monokai-theme                               ;; monokai theme
    zenburn-theme                               ;; zenburn color theme
    material-theme                              ;; material themes
    color-theme-sanityinc-tomorrow              ;; tomorrow themes
    color-theme-sanityinc-solarized             ;; solarized themes
    cyberpunk-theme                             ;; cyberpunk theme for emacs
    majapahit-theme                             ;; color theme with a dark and light versions
    zerodark-theme                              ;; dark medium-contrast theme
    flatui-theme                                ;; color theme based on flat colors
    ;;;;;; project setup and configuration      ;;;;;;
    projectile                                  ;; Project Interaction Library for Emacs
    helm-projectile                             ;; Helm UI for Projectile
    ;;;;;; company auto completions frameworks  ;;;;;;
    company                                     ;; cmopany autocompletion modes
    ;;;;;; company backends for completion      ;;;;;;
    company-jedi                                ;; company jedi mode for python
    company-distel                              ;; company distel mode for erlang
    company-erlang                              ;; company backend for erlang, ivy-erlang-complete
    distel-completion-lib                       ;; distel-completion is needed for company-distel
    company-dict                                ;; backend that emulates ac-source-dictionary
    company-quickhelp                           ;; documentation popup for company
    helm-company                                ;; helm interface for company-mode
    company-math                                ;; backend for for math unicode symbols and latex tags
    company-flx                                 ;; Flx fuzzy matching for company
    company-irony                               ;; completion backend for irony-mode
    company-irony-c-headers                     ;; backend for C/C++ header files with irony-mode
    company-c-headers                           ;; auto-completion for C/C++ headers using Company
    ;company-ycm                                ;; Emacs client for the YCM code-completion engine
    ;;;;;; auto-complete family                 ;;;;;;
    auto-complete                               ;; auto completion for gnu emacs
    auto-complete-distel                        ;; auto completion distel for erlang
    ac-haskell-process                          ;; haskell completion source for Emacs auto-complete
    auto-complete-nxml                          ;; auto-completion on nXml mode
    ac-cider                                    ;; clojure completion source
    auto-complete-clang                         ;; auto complete source for clang
    auto-complete-c-headers                     ;; auto-complete source for C/C++ header files
    ;;;;;; some utilities                       ;;;;;;
    parent-mode                                 ;; get major mode's parent modes
    ; ido                                       ;; IDO mode
    ; smex                                      ;; M-x interface with Ido-style fuzzy matching
    ;;;;;; essential utilities                  ;;;;;;
    smartparens                                 ;; parenthesis management
    evil-smartparens                            ;; evil integration for smartparens
    paredit                                     ;; minor mode for editing parentheses
    evil-paredit                                ;; evil extension for paredit
    ;;;;;; documentation and help               ;;;;;;
    markdown-mode                               ;; markdown language support
    ; auctex                                    ;; AUCTEX and LATEX
    ;;;;;; on the fly syntax checkers           ;;;;;;
    ;;;;;; flycheck family                      ;;;;;;
    flycheck                                    ;; flycheck on the fly syntax checker
    flycheck-color-mode-line                    ;; flycheck colors for highlighting errors
    flycheck-pos-tip                            ;; flycheck errors display in tooltip
    flycheck-tip                                ;; show flycheck/flymake errors by tooltip
    helm-flycheck                               ;; show flycheck errors with helm
    flycheck-haskell                            ;; haskell syntax checker
    flycheck-elixir                             ;; flycheck checker for elixir files
    flycheck-mix                                ;; flycheck elixir mix support
    flycheck-clojure                            ;; flycheck clojure support
    popup                                       ;; show popup for flycheck
    flycheck-irony                              ;; flycheck c/c++ support via Irony
    ;;;;;; flymake family                       ;;;;;;
    flymake-easy                                ;; flymake on the fly syntax checker
    flymake-python-pyflakes                     ;; flymake handler for syntax-checking Python source code using pyflakes or flake8
    flymake-hlint                               ;; linting for haskell language
    flymake-cursor                              ;; show flymake errors in mini buffer
    flymake-google-cpplint                      ;; comply with the Google C++ Style Guide on Emacs with flymake
    ;;;;;; styling and formatting               ;;;;;;
    google-c-style                              ;; google's c/c++ style for c-mode
    ;;;;;; org modes                            ;;;;;;
    org                                         ;; org-mode setup
    org-bullets                                 ;; org mode with bullets
    latex-pretty-symbols                        ;; unicode display of characters
    ;;;;;; git integration                      ;;;;;;
    ;magit                                      ;; git status
    git-gutter                                  ;; Emacs port of GitGutter
    ;;;;;; language and IDE setup               ;;;;;;
    ;;;;;; python 3 programming modes           ;;;;;;
    virtualenvwrapper                           ;; virtualenv wrapper for python
    jedi                                        ;; python jedi IDE
    elpy                                        ;; python elpy IDE
    python-pylint                               ;; python linter
    py-yapf                                     ;; python yapf
    pyvenv                                      ;; python virtual environment interface for Emacs
    py-autopep8                                 ;; integrate autopep8 into Emacs
    ;;;;;; haskell programming modes            ;;;;;;
    haskell-mode                                ;; haskell language support
    company-ghc                                 ;; haskell company auto-completion
    company-ghci                                ;; a company backend for haskell
    company-cabal                               ;; cabal company support
    shm                                         ;; structured haskell mode
    haskell-snippets                            ;; haskell language snippets
    hindent                                     ;; haskell code indenting
    hi2                                         ;; for haskell-indentation, 2nd try
    ghc                                         ;; haskell ghc
    ; intero                                    ;; complete dev environment for haskell
    ;;;;;; erlang laguage support               ;;;;;;
    erlang                                      ;; erlang emacs plugin
    ivy-erlang-complete                         ;; context sensitive completion for erlang
    ; edts                                      ;; erlang development ide
    ;;;;;; elixir language                      ;;;;;;
    elixir-mode                                 ;; major mode for editing elixir files
    alchemist                                   ;; elixir tooling integration into Emacs
    ac-alchemist                                ;; auto-complete source for alchemist
    ;;;;;; scala development with ensime        ;;;;;;
    ensime                                      ;; ENhanced Scala Interaction Mode for Emacs
    scala-mode                                  ;; scala
    sbt-mode                                    ;; Emacs mode for interacting with scala sbt and projects
    ;;;;;; go development support               ;;;;;;
    go-mode                                     ;; major mode for go programming
    go-eldoc                                    ;; eldoc for go-mode
    go-autocomplete                             ;; auto completion backend for go
    golint                                      ;; lint for go source
    ;;;;;; c/c++ language support               ;;;;;;
    irony                                       ;; a c/c++ minor mode for Emacs powered by libclang
    irony-eldoc                                 ;; eldoc support in irony-mode
    clang-format                                ;; code formatting
    ;;;;;; yasnippets package                   ;;;;;;
    yasnippet                                   ;; Yet another snippet extension
    helm-c-yasnippet                            ;; Helm source for yasnippet
    elixir-yasnippets                           ;; yasnippets for elixir
    clojure-snippets                            ;; snippets for clojure
    ;;;;;; important and useful utilities       ;;;;;;
    helm                                        ;; incremental completion and selection narrowing framework
    helm-core                                   ;; development files for Helm
    ;helm-describe-modes                        ;; Helm interface to Emacs’s describe-mode
    ;helm-gtags                                 ;; gnu global helm interface
    ;;;;;; essential packs and tools            ;;;;;;
    ecb                                         ;; emacs code browser
    buffer-move                                 ;; move buffer
    neotree                                     ;; a tree plugin like NerdTree for Vim
    ;;;;;; essential utilities                  ;;;;;;
    highlight-symbol                            ;; automatic and manual symbol highlighting for Emacs
    xah-math-input                              ;; show math input symbols
    ;;;;;; icon displays                        ;;;;;;
    ; mode-icons                                ;; show icons for modes
    all-the-icons                               ;; package for showing various icons
    ;;;;;; editing and keyboard mappings        ;;;;;;
    key-chord                                   ;; map pairs of simultaneously pressed keys to commands
    diminish                                    ;; diminished modes are minor modes with no modeline display
    multiple-cursors                            ;; multiple cursors for emacs
    iedit                                       ;; edit multiple regions simultaneously in a buffer or a region
    ;;;;;; web app(s), java script and json     ;;;;;;
    web-mode                                    ;; major-mode for editing web templates
    js2-refactor                                ;; javascript refactoring library
    tern                                        ;; JavaScript code analyzer
    tern-auto-complete                          ;; js tooling auto-complete
    js2-mode                                    ;; Improved JavaScript editing mode
    ac-js2                                      ;; Javascript auto-completion
    json-mode                                   ;; major mode for json editing
    ;;;;;; text and file utilities              ;;;;;;
    popwin                                      ;; popup window manager
    ;;;;;; miscellaneous utilities              ;;;;;;
    wttrin                                      ;; weather information from wttr.in
    esup                                        ;; emacs startup profiler (https://github.com/jschaf/esup)
    which-key                                   ;; displays available keybindings in popup
    discover-my-major                           ;; key bindings and their meaning for the current Emacs major mode
    sunshine                                    ;; weather and forecast information
    manage-minor-mode                           ;; manage minor modes on a dedicated buffer
    ;;;;;; vim emulation                        ;;;;;;
    evil                                        ;; Extensible Vi layer for Emacs.
    undo-tree                                   ;; Treat undo history as a tree (evil dependency)
    goto-chg                                    ;; goto last change (evil dependency)
    evil-leader                                 ;; let there be <leader>
    evil-surround                               ;; emulate surround.vim from Vim
    ;;;;;; clojure programming modes            ;;;;;;
    clojure-mode                                ;; Emacs support for clojure
    clojure-mode-extra-font-locking             ;; Extra font-locking for Clojure mode
    helm-cider                                  ;; helm interface for cider
    helm-clojuredocs                            ;; searching for help in clojurdocs.org with helm
    cider                                       ;; Clojure Interactive Development Environment that Rocks
    ;;;;;; YouCompleteMe Configurations         ;;;;;;
    ycmd                                        ;; emacs bindings to the ycmd completion server
    company-ycmd                                ;; company mode backend for ycmd
    flycheck-ycmd                               ;; flycheck integration for ycmd
    ;;;;;; indentation and text editing         ;;;;;;
    ;aggressive-indent                          ;; minor mode for code indentation
  )
  "A list of packages that will be installed if not present when firing Emacs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Now add the above packages to the load-path               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (file-exists-p save-dir)
  (make-directory save-dir))
(add-to-list 'load-path module-dir)
(add-to-list 'load-path vendor-dir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Load the requires packages in the vendor/ directory           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc 'load (directory-files vendor-dir nil "^[^#].*el$"))
;;
; Load the requires packages in the modules/ directory
; modules contain custom init files which can be loaded
; after all the packages are installed, hence commented
;;
;(mapc 'load (directory-files module-dir nil "^[^#].*el$"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to check if all listed packages are installed. return true when   ;;
;; package is not installed. When Emacs boots, check to make sure all the     ;;
;; packages defined in required-packages are installed. If not ELPA kicks in. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun packages-installed-p()
  "Load each package specified in the required-packages section."
  (loop for pkg in required-packages
        when (not (package-installed-p pkg)) do (return nil)
            finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if not all the packages which are listed are installed, check one by one ;;;
;;; and install the missing ones.                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" "package refresh done.")
  ; install the missing packages
  (dolist (pkg required-packages)
    (when (not (package-installed-p pkg))
      (message ">>> installing missing package -> %s" pkg)
      (package-install pkg))))


(provide 'required-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; package loading of all the custom el files which contains customized
;; settings for each major/minor modes as well as any other packages
;; currently the below are all customized supported configurations.
;;
;; miscellaneous settings
;; themes
;; company
;; company-quickhelp
;; auto-complete
;; semanticdb configuration
;; flycheck
;; flyspell
;; helm
;; smart parentheses
;; parenthesis edit
;; rainbow-mode
;; rainbow identifiers
;; rainbow-delimiters
;; hihlight-symbols
;; fringe
;; neotree
;; popup window
;; Emacs code browser ecb
;; org bullets
;; multiple-cursors
;; code snippets with yas
;; gitgutter-config
;; weather info
;; which-key
;; evil
;; xslt
;; xml using nxml
;; aggressive indentation
;; haskell
;; erlang
;; python3
;; scala
;; elixir
;; go
;; c/c++
;; web
;; clojure
;; markdown
;; ycm-config.el
;; projectile
;; delight and dim
;; latex configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar configs
    '(
      "misc-config"
      "themes-config"
      "company-config"
      "ac-complete-config"
      "semantic-config"
      "flycheck-config"
      "flyspell-config"
      "helm-settings-config"
      "rbow-config"
      "rbow-identifiers-config"
      "rainbow-delims-config"
      "smart-config"
      "paredit-config"
      "highlight-symbol-config"
      "fringe-config"
      "neotree-config"
      "popwin-config"
      "ecb-config"
      "org-config"
      "multiple-cursors-config"
      "yasnippets-config"
      "gitgutter-config"
      "weather-config"
      "whichkey-config"
      "evil-config"
      "xslt-process-config"
      "cpp-config"
      "python-config"
      "haskell-config"
      "erlang-config"
      "elixir-config"
      "scala-config"
      "go-config"
      "clojure-config"
      "nxml-config"
      "web-config"
      "markdown-config"
      "ycm-config"
      "projectile-config"
      "delighted-config"
      "tex-config"
      )
    "Configuration files which follow the modules/pkgname-config.el format."
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; loop through each and load the configured custom packages              ;;;;
;;;; each configuration file has a format of name-config.el                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "modules/"
                       name ".el")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load packages from custom path                                         ;;;;
;;;; contains packages not in elpa/melpa/marmalade/gnu/org                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar custom-load-paths
  '(; "erlang/elisp"        ;; erlang lisp modules
    ; "xslide"              ;; xml and xslt syntax, customizations
    "xslt-process/lisp"     ;; xslt processor ide
    )
  "Custom load paths that do not follow the normal vendor/elisp/module-name.el format."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; loop through the custom lisp                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for location in custom-load-paths
      do (add-to-list 'load-path
             (concat (file-name-directory (or load-file-name
                                              (buffer-file-name)))
                     "vendor/"
                     location)))


(provide 'aqua-package-repos)
;;; aqua-package-repos.el ends here
