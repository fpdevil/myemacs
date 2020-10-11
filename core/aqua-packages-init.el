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
;;; Updated    : 13 Sep 2020
;;;


;;** required default standard libraries
(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'package)

(message "aqua-package.el %s" (or load-file-name (buffer-file-name)))
(message "directory %s" (file-name-directory (or load-file-name (buffer-file-name))))

;;-- defvar is the correct way to declare global variables
;;-- setq is supposed to be use just to set variables and not create them.
(defvar required-packages
  '(
    ;;;;;; tools section                         ;;;;;;
    async                                       ;; asynchronous processing in Emacs
    ;;;;;; appearance and visual customization's ;;;;;;
    powerline                                   ;; powerline smart mode
    smart-mode-line                             ;; powerful and beautiful mode-line
    smart-mode-line-powerline-theme             ;; powerline theme for sml
    spaceline                                   ;; powerline theme in spacemacs format
    dim                                         ;; mode-line names of major/minor modes
    rainbow-delimiters                          ;; colorful modes (delimiters and color codes)
    rainbow-mode                                ;; colored identifiers
    rainbow-identifiers                         ;; colored identifiers
    airline-themes                              ;; fancy vim airline themes
    ;;;;;;  color themes for emacs              ;;;;;;
    color-theme                                 ;; install color themes
    molokai-theme                               ;; molokai theme
    sublime-themes                              ;; sublime themes
    darkokai-theme                              ;; dark theme based on monokai
    dracula-theme                               ;; dracula dark theme
    moe-theme                                   ;; group of moe themes
    monokai-theme                               ;; monokai theme
    zenburn-theme                               ;; zenburn color theme
    material-theme                              ;; material themes
    color-theme-sanityinc-tomorrow              ;; tomorrow themes
    color-theme-sanityinc-solarized             ;; solarized themes
    solarized-theme                             ;; bozhidaar solarized for emacs
    cyberpunk-theme                             ;; cyberpunk theme for emacs
    cherry-blossom-theme                        ;; soothing dark theme
    majapahit-theme                             ;; color theme with a dark and light versions
    zerodark-theme                              ;; dark medium-contrast theme
    flatui-theme                                ;; color theme based on flat colors
    spacemacs-theme                             ;; spacemacs light and dark themes
    gruvbox-theme                               ;; retro-groove colour theme
    paper-theme                                 ;; minimal color theme
    twilight-bright-theme                       ;; light theme based on twilight
    twilight-theme                              ;; twilight theme
    tango-plus-theme                            ;; light theme based on tango
    doom-themes                                 ;; pack of modern color-themes
    afternoon-theme                             ;; dark theme with deep blue background
    molokai-theme                               ;; Yet another molokai theme for Emacs 24
    ;;;;;; project setup and configuration      ;;;;;;
    ;; projectile                               ;; Project Interaction Library for Emacs
    ;; helm-projectile                          ;; Helm UI for Projectile
    ;;;;;; company auto completions frameworks  ;;;;;;
    company                                     ;; cmopany autocompletion modes
    company-try-hard                            ;; get all completions from company backends
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
    ;;company-tern                              ;; tern backend for company-mode
    company-ghc                                 ;; haskell company auto-completion
    company-ghci                                ;; a company backend for haskell
    company-cabal                               ;; cabal company support
    company-web                                 ;; company backend for ac-html
    company-ycmd                                ;; company mode backend for ycmd
    company-lsp                                 ;; company mode backend for lsp
    ;;company-go                                ;; Company complete for go
    ;;company-ycm                               ;; Emacs client for the YCM code-completion engine
    ;;;;;; auto-complete family                 ;;;;;;
    auto-complete                               ;; auto completion for gnu emacs
    auto-complete-distel                        ;; auto completion distel for erlang
    ac-haskell-process                          ;; haskell completion source for Emacs auto-complete
    auto-complete-nxml                          ;; auto-completion on nXml mode
    ac-cider                                    ;; clojure completion source
    auto-complete-clang                         ;; auto complete source for clang
    auto-complete-clang-async                   ;; auto complete source for asynchronous clang
    auto-complete-c-headers                     ;; auto-complete source for C/C++ header files
    ac-slime                                    ;; auto completion for slime
    fuzzy                                       ;; fuzzy matching utilities for GNU Emacs
    ac-capf                                     ;; auto-complete source of completion-at-point
    ;;;;;; Language Server Protocol             ;;;;;;
    lsp-mode
    lsp-ui
    lsp-ivy
    lsp-treemacs
    dap-mode                                    ;; Debug Adapter Procol
    ;; taken care of in lsp-config.el
    ;;;;;; some utilities                       ;;;;;;
    parent-mode                                 ;; get major mode's parent modes
    ;; exec-path-from-shell                     ;; make Emacs use the $PATH set up by the user's shell
    ;;better-defaults                           ;; better defaults for Emacs
    ;;;;;; essential utilities                  ;;;;;;
    smartparens                                 ;; parenthesis management
    ;;;;;; documentation and help               ;;;;;;
    markdown-mode                               ;; markdown language support
    ;; auctex                                   ;; AUCTEX and LATEX
    graphviz-dot-mode                           ;; dot language graphviz graphs (for erlang .dotfiles)
    ;;;;;; on the fly syntax checkers           ;;;;;;
    ;;;;;; flycheck family                      ;;;;;;
    flycheck                                    ;; flycheck on the fly syntax checker
    flycheck-color-mode-line                    ;; flycheck colors for highlighting errors
    flycheck-pos-tip                            ;; flycheck errors display in tooltip
    flycheck-tip                                ;; show flycheck/flymake errors by tooltip
    helm-flycheck                               ;; show flycheck errors with helm
    flycheck-haskell                            ;; haskell syntax checker
    flycheck-elixir                             ;; flycheck checker for elixir files
    ;;flycheck-mix                              ;; flycheck elixir mix support
    flycheck-clojure                            ;; flycheck clojure support
    flycheck-irony                              ;; flycheck c/c++ support via Irony
    flycheck-plantuml                           ;; flycheck for plantuml automatic syntax errors
    ;;flycheck-rebar3                           ;; flycheck integration for rebar3 projects
    ;;;;;; spell checking                       ;;;;;;
    flyspell-lazy                               ;; improve Emacs flyspell responsiveness using idle timers
    helm-flyspell                               ;; helm extension for correcting words with flyspell
    ;;;;;; flymake family                       ;;;;;;
    flymake                                     ;; flymake package
    flymake-python-pyflakes                     ;; flymake handler for syntax-checking Python source code using pyflakes or flake8
    flymake-hlint                               ;; linting for haskell language
    flymake-cursor                              ;; show flymake errors in mini buffer
    flymake-google-cpplint                      ;; comply with the Google C++ Style Guide on Emacs with flymake
    ;;;;;; styling and formatting               ;;;;;;
    google-c-style                              ;; google's c/c++ style for c-mode
    ;;;;;; org modes                            ;;;;;;
    org                                         ;; org-mode setup
    org-plus-contrib                            ;; plus all contribs files
    org-ac                                      ;; auto completion for org
    ox-reveal                                   ;; for reveal.js presentations through org-mode
    org-tree-slide                              ;; presentations tools for org-mode
    ox-html5slide                               ;; export org-mode to html5 slide
    plantuml-mode                               ;; editing PlantUML sources
    latex-pretty-symbols                        ;; unicode display of characters
    org-download                                ;; image drag and drop for org-mode
    biblio                                      ;; browsing and fetching references
    org-easy-img-insert                         ;; insert images from web
    ob-http                                     ;; make http request within org-mode babel
    ob-go                                       ;; go babel
    ;; ob-scala                                 ;; org scala
    epresent                                    ;; simple presentation mode for Emacs Org-mode
    htmlize                                     ;; convert buffer text and decorations to HTML
    org-bullets                                 ;; org mode with bullets
    ;;;;;; git integration                      ;;;;;;
    ;;magit                                     ;; git status
    git-gutter                                  ;; Emacs port of GitGutter
    ibuffer-vc                                  ;; Group ibuffer's list by VC project, or show VC status
    ;;;;;; language and IDE setup               ;;;;;;
    ;;;;;; python 3 programming modes           ;;;;;;
    virtualenvwrapper                           ;; virtualenv wrapper for python
    jedi                                        ;; python auto-completion for Emacs
    jedi-core                                   ;; common code of jedi.el and company-jedi.el
    elpy                                        ;; python elpy IDE
    pylint                                      ;; python linter
    pyvenv                                      ;; python virtual environment interface for Emacs
    py-autopep8                                 ;; integrate autopep8 into Emacs
    sphinx-doc                                  ;; sphinx style doc strings for python code
    py-isort                                    ;; integrates isort into Emacs
    pyimpsort                                   ;; organize python imports
    ;;pydoc-info                                ;; better python support for info lookup
    ;;python-pylint                             ;; python linter
    ;;;;;; haskell programming modes            ;;;;;;
    haskell-mode                                ;; haskell language support
    ghci-completion                             ;; completion for GHCi commands in inferior-haskell buffers
    haskell-snippets                            ;; haskell language snippets
    hindent                                     ;; haskell code indenting
    ;;hi2                                       ;; for haskell-indentation, 2nd try
    ;;;;;; erlang laguage support               ;;;;;;
    erlang                                      ;; erlang emacs plugin
    edts                                        ;; Erlang Development Tool Suite
    ;;ivy-erlang-complete                       ;; context sensitive completion for erlang
    ;;;;;; elixir language                      ;;;;;;
    elixir-mode                                 ;; major mode for editing elixir files
    alchemist                                   ;; elixir tooling integration into Emacs
    ac-alchemist                                ;; auto-complete source for alchemist
    ;;;;;; scala development with ensime        ;;;;;;
    ;;ensime                                    ;; ENhanced Scala Interaction Mode for Emacs
    scala-mode                                  ;; scala
    sbt-mode                                    ;; Emacs mode for interacting with scala sbt and projects
    ;;;;;; go development support               ;;;;;;
    go-mode                                     ;; major mode for go programming
    go-guru                                     ;; go guru analysis tool
    go-eldoc                                    ;; eldoc for go-mode
    golint                                      ;; lint for go source
    go-autocomplete                             ;; auto completion backend for go
    ;;;;;; rust development support             ;;;;;;
    rust-mode                                   ;; major mode for rust
    cargo                                       ;; rust package manager
    racer                                       ;; support for rust code completion
    company-racer                               ;; company code completion for rust
    ac-racer                                    ;; auto complete mode for rust
    flycheck-rust                               ;; syntax checking for rust
    toml-mode                                   ;; toml mode file handling
    ;;rustic                                    ;; add on to rust-mode
    ;;;;;; c/c++ language support               ;;;;;;
    irony                                       ;; a c/c++ minor mode for Emacs powered by libclang
    irony-eldoc                                 ;; eldoc support in irony-mode
    clang-format                                ;; code formatting
    c-eldoc                                     ;; description of the arguments to C functions
    function-args                               ;; C++ completion for GNU Emacs
    cmake-project                               ;; Integrates CMake build process with Emacs
    realgud                                     ;; extensible debugger for emacs
    function-args
    cpputils-cmake                              ;; real-time c++ syntac check & IntelliSense with CMake
    ;;;;;; yasnippets package                   ;;;;;;
    yasnippet                                   ;; Yet another snippet extension
    yasnippet-snippets
    helm-c-yasnippet                            ;; Helm source for yasnippet
    elixir-yasnippets                           ;; yasnippets for elixir
    clojure-snippets                            ;; snippets for clojure
    ;;;;;; important and useful utilities       ;;;;;;
    ;;helm                                      ;; incremental completion and selection narrowing framework
    ;;helm-core                                 ;; development files for Helm
    ;;helm-describe-modes                       ;; Helm interface to Emacs’s describe-mode
    ;;helm-gtags                                ;; gnu global helm interface
    ;;imenu-list                                ;; show the current buffer's imenu entries
    ;;;;;; essential packs and tools            ;;;;;;
    ecb                                         ;; emacs code browser
    neotree                                     ;; a tree plugin like NerdTree for Vim
    dired-imenu                                 ;; imenu binding for dired mode
    volatile-highlights                         ;; visual feedback on operations
    ;;golden-ratio                              ;; auto re-size Emacs windows Tool golden ratio
    ;;;;;; essential utilities                  ;;;;;;
    highlight-symbol                            ;; automatic and manual symbol highlighting for Emacs
    xah-math-input                              ;; show math input symbols
    fiplr                                       ;; Emacs Fuzzy Find in Project Package
    ;;;;;; icon displays                        ;;;;;;
    all-the-icons                               ;; package for showing various icons
    ;;mode-icons                                ;; show icons for modes
    ;;;;;; editing and keyboard mappings        ;;;;;;
    key-chord                                   ;; map pairs of simultaneously pressed keys to commands
    diminish                                    ;; diminished modes are minor modes with no mode-line display
    delight                                     ;; customise how major and minor modes appear in modeline
    multiple-cursors                            ;; multiple cursors for emacs
    ace-mc                                      ;; Add Multiple Cursors using Ace Jump
    iedit                                       ;; edit multiple regions simultaneously in a buffer or a region
    ;;;;;; web, html, java script and json      ;;;;;;
    web-mode                                    ;; major-mode for editing web templates
    js-comint                                   ;; repl integration
    rjsx-mode                                   ;; react js
    js2-refactor                                ;; javascript refactoring library
    js2-highlight-vars                          ;; highlight variables
    tern                                        ;; JavaScript code analyzer
    tern-auto-complete                          ;; js tooling auto-complete
    json-navigator                              ;; View and navigate JSON structures
    js2-mode                                    ;; Improved JavaScript editing mode
    js3-mode                                    ;; chimeric fork of js2-mode and js-mode
    js-doc                                      ;; Insert JsDoc style comment easily
    jsfmt                                       ;; formatting, searching, and rewriting javascript
    ac-js2                                      ;; Javascript auto-completion
    json-mode                                   ;; major mode for json editing
    coffee-mode                                 ;; major mode for CoffeeScript
    ac-emmet                                    ;; auto-complete sources for emmet-mode
    emmet-mode                                  ;; emmet support for emacs
    web-beautify                                ;; Format HTML, CSS and JavaScript/JSON by js-beautify
    json-navigator                              ;; View and navigate JSON structures
    livid-mode
    ;;tern-context-coloring                     ;; adding scope coloring
    ;;tj-mode                                   ;; Highlight JavaScript with Tern
    ;;indium                                    ;; JS Awesome Development Environment formerly jade
    ;;tidy                                      ;; interface to html tidy program
    ;;;;;; window, text and file utilities      ;;;;;;
    popwin                                      ;; popup window manager
    popup                                       ;; show popup for flycheck
    clippy                                      ;; show tooltip with function documentation at point
    quick-peek                                  ;; inline window doc popup
    drag-stuff                                  ;; Drag stuff around in Emacs
    ;;;;;; miscellaneous utilities              ;;;;;;
    wttrin                                      ;; weather information from wttr.in
    esup                                        ;; emacs startup profiler (https://github.com/jschaf/esup)
    which-key                                   ;; displays available keybindings in popup
    guide-key                                   ;; displays key bindings
    guide-key-tip                               ;; guide-key with pos-tio
    beacon                                      ;; follows your cursor around
    discover-my-major                           ;; key bindings and their meaning for the current Emacs major mode
    sunshine                                    ;; weather and forecast information
    manage-minor-mode                           ;; manage minor modes on a dedicated buffer
    know-your-http-well                         ;; Look up the meaning of HTTP metadata
    zzz-to-char                                 ;; Fancy version of ‘zap-to-char’ command
    ;;;;;; vim emulation                        ;;;;;;
    evil                                        ;; Extensible Vi layer for Emacs.
    undo-tree                                   ;; Treat undo history as a tree (evil dependency)
    goto-chg                                    ;; go to last change (evil dependency)
    evil-leader                                 ;; let there be <leader>
    evil-surround                               ;; emulate surround.vim from Vim
    evil-mc                                     ;; multiple cursors for evil-mode
    evil-smartparens                            ;; evil integration for smartparens
    evil-paredit                                ;; evil extension for paredit
    evil-indent-textobject                      ;; Textobject for evil based on indentation
    evil-commentary                             ;; commenting the blocks with evil
    evil-exchange                               ;; port of vim-exchange
    evil-anzu                                   ;; anzu for Evil
    evil-ediff                                  ;; make ediff a little more evil
    evil-magit                                  ;; magit for evil
    evil-avy                                    ;; evil motion with avy: choose where after which
    evil-matchit                                ;; Vim matchit ported into Emacs
    evil-visualstar                             ;; Start a * or # search from the visual selection (aka vim)
    evil-numbers                                ;; Increment and decrement numbers in Emacs
    ;;;;;; clojure programming modes            ;;;;;;
    clojure-mode                                ;; Emacs support for clojure
    clojure-mode-extra-font-locking             ;; Extra font-locking for Clojure mode
    helm-cider                                  ;; helm interface for cider
    helm-clojuredocs                            ;; searching for help in clojuredocs.org with helm
    cider                                       ;; Clojure Interactive Development Environment that Rocks
    ;;;;;; You Complete Me  Completion          ;;;;;;
    ycmd                                        ;; emacs bindings to the ycmd completion server
    flycheck-ycmd                               ;; flycheck integration for ycmd
    ;;;;;; indentation and text editing         ;;;;;;
    ;;aggressive-indent                         ;; minor mode for code indentation
    )
  "A list of packages that will be installed if not present when firing Emacs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load packages from custom path                                         ;;;;
;;;; contains packages not in elpa/melpa/marmalade/gnu/org                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar custom-load-paths
  '(
    "xslide"                            ;; xml and xslt syntax, customization's
    "xslt-process/lisp"                 ;; xslt processor ide
    "elisp/elisp-format"                ;; format elisp code
    "elisp/ps2pdf"                      ;; convert ps to pdf
    "psgml"                             ;; markup languages...
    "cpp-addon"                         ;; addon to cc-mode
    "flycheck-google-cpplint"           ;; google c++ style checker for flycheck
    "emacs-clang-complete-async/elisp"
    "elisp/ox-manuscript"
    ;;"lsp/eglot"
    ;;"lsp/elisp-json-rpc"
    ;;"dircolors"                       ;; colored buffer
    ;;"javascript/node-ac"              ;; node-js auto-complete package
    )
  "Custom load paths that do not follow the normal vendor/elisp/module-name.el format.")

;;----------------------------------------------------------------------------
;;** initialize all the defined packages
;;----------------------------------------------------------------------------
(defun initialize-package ()
  "Initializing the packages."
  (unless nil ;package--initialized
    ;; optimization, no need to activate all the packages so early
    (setq package-enable-at-startup nil)
    (package-initialize)))

(initialize-package)

;;----------------------------------------------------------------------------
;; Package repositories (gnu, melpa, melpa-stable and marmalade)
;;----------------------------------------------------------------------------
(setq package-archives
      '(
        ;; uncomment/comment the below line for GNU ELPA
        ("gnu"            . "http://elpa.gnu.org/packages/")
        ("melpa"          . "https://melpa.org/packages/")
        ("melpa-stable"   . "https://stable.melpa.org/packages/")
        ("melpa-unstable" . "http://unstable.melpa.org/packages/")
        ("elpy"           . "http://jorgenschaefer.github.io/packages/")
        ("org"            . "http://orgmode.org/elpa/")
        ;;("marmalade"      . "https://marmalade-repo.org/packages/")
        ))

;; use openssl certificate authorities
(require 'gnutls)
(require 'tls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
(with-eval-after-load 'tls
  (push "/usr/local/etc/openssl/cert.pem" gnutls-trustfiles))

(setq tls-checktrust t)
(setq package-check-signature nil)
(setq gnutls-verify-error t)

;; set it to `t' in order to use a safer HTTPS to download packages
(defvar melpa-use-https-repo nil
  "By default, HTTP is used to download packages.
But you may use safer HTTPS instead.")

;;----------------------------------------------------------------------------
;;** if on Emacs 24.4 or newer, if so, use the pinned package feature
;;----------------------------------------------------------------------------
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '(
          (elpy                  . "elpy")
          (highlight-indentation . "elpy") ;; fixes error in elpy 1.6
          (org                   . "org")
          (org-plus-contrib      . "org")
          (org-download          . "melpa")
          (posframe              . "melpa")
          (jedi                  . "melpa-stable")
          (jedi-core             . "melpa-stable")
          (company-jedi          . "melpa-stable")
          (markdown-mode         . "melpa-stable")
          (delight               . "gnu")
          (lsp-mode              . "melpa-stable")
          (dap-mode              . "melpa-stable")
          (smart-mode-line       . "melpa")
          (ensime                . "melpa-stable")
          (ac-capf               . "melpa-stable")
          (clj-refactor          . "melpa-unstable") ;; note from http://planet.clojure.in/
          (monroe                . "melpa-stable")
          (smart-mode-line       . "melpa-stable")
          (web-mode              . "melpa")
          (counsel               . "melpa")
          (which-key             . "melpa"))))

(setq package-menu-hide-low-priority t)

;;----------------------------------------------------------------------------
;;** start refreshing all the defined packages
;;----------------------------------------------------------------------------
(unless (file-exists-p package-user-dir)
  (message "No packages exist yet, refreshing archives.")
  (package-refresh-contents))

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

;;--------------------------------------------------------------------------
;;** pretty highlighting for the require-package
;;--------------------------------------------------------------------------
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(require-package\\)\\>" 1 font-lock-builtin-face)))

;;--------------------------------------------------------------------------
;;**  function to check if all listed packages are installed. return true when
;;**  package is not installed. When Emacs boots, check to make sure all the
;;**  packages defined in required-packages are installed. If not ELPA kicks in
;;--------------------------------------------------------------------------
(defun aqua-packages-installed-p ()
  "Check if packages are installed or not."
  (cl-loop for p in required-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

;;--------------------------------------------------------------------------
;; [straight.el] - package manager bootstrapping straight.el
;;--------------------------------------------------------------------------
(unless (featurep 'straight)
  (defvar bootstrap-version)

  (let ((bootstrap-file (concat user-emacs-directory
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; load the straight.el version of use-package
(defvar straight-use-package-by-default)
(straight-use-package 'use-package)
(setq straight-check-for-modifications '(find-when-checking))
;; tell straight to use use-package by default
;; (setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(if nil                             ; set to t when need to debug init
    (progn
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
      (require 'use-package))
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(setq use-package-always-defer t)

;;--------------------------------------------------------------------------
;;** [use-package] - AddOn package manager for package installation
;;--------------------------------------------------------------------------
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-and-compile
;;   (setq use-package-verbose t)
;;   (setq use-package-always-defer t)
;;   (setq use-package-always-ensure t)
;;   (setq use-package-expand-minimally t)
;;   (setq use-package-compute-statistics t)
;;   (setq use-package-enable-imenu-support t))

;; (eval-when-compile
;;   (require 'use-package))

;; note on use-package declarations:
;;     :init          Code to run BEFORE package has been loaded.
;;     :config        Code to run AFTER package has been loaded.

;;----------------------------------------------------------------------------
;; quelpa package manager for the Emacs
;;----------------------------------------------------------------------------
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; do not update Quelpa or MELPA repo on initialization as it slows down launch time
(setq quelpa-upgrade-p nil
      quelpa-update-melpa-p nil)

;;----------------------------------------------------------------------------
;;** for GC and benchmarking
;;----------------------------------------------------------------------------
(straight-use-package 'gcmh)
(use-package gcmh
  :demand t
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold #x800000
        gcmh-high-cons-threshold most-positive-fixnum
        gcmh-idle-delay          3600)
  :config
  (gcmh-mode))

(require-package 'benchmark-init)
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(add-hook 'after-init-hook 'benchmark-init/activate)

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
(autoload-lazy-major-mode "CMakeLists\\.txt'"   cmake-mode)
(autoload-lazy-major-mode "PKGBUILD\\'"         pkgbuild-mode)
(autoload-lazy-major-mode "\\.jl\\'"            julia-mode)
(autoload-lazy-major-mode "\\.go\\'"            go-mode)
(autoload-lazy-major-mode "\\.vim\\(rc\\)?\\'"  vimrc-mode)
(autoload-lazy-major-mode "\\.csv$"             csv-mode)
(autoload-lazy-major-mode "\\.elm$\\'"          elm-mode)
(autoload-lazy-major-mode "\\.groovy$\\'"       groovy-mode)
(autoload-lazy-major-mode "\\.lua$\\'"          lua-mode)
(autoload-lazy-major-mode "\\.cmake$\\'"        cmake-mode)
(autoload-lazy-major-mode "\\.php$\\'"          php-mode)
(autoload-lazy-major-mode "\\.proto$\\'"        protobuf-mode)
(autoload-lazy-major-mode "\\.rs$\\'"           rust-mode)
(autoload-lazy-major-mode "\\.swift$\\'"        swift-mode)
(autoload-lazy-major-mode "\\.coffee$\\'"       coffee-mode)
(autoload-lazy-major-mode "Dockerfile\\'"       dockerfile-mode)
(autoload-lazy-major-mode "\\.epub\\'"          nov-mode)
(autoload-lazy-major-mode "\\.\\(yml\\|yaml\\)$" yaml-mode)

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

(provide 'aqua-packages-init)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; aqua-packages-init.el ends here
