;;; package --- list of packages to be installed by Aquamacs
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-packages.el
;;; description: This file contains new or missing packages to be installed
;;               during Emacs bootstrap.  Any new packages to be installed
;;               for Emacs should be defined as a part of the section defined
;;               under required-packages.  Any packages not available as ready
;;               packages should be in the vendor directory.
;;;
;;; Code:
;;; Updated    : 17 Nov 2017
;;;

;;-- provide the required default standard libraries
(eval-when-compile (require 'cl))

(message "aqua-package.el %s"  (or load-file-name (buffer-file-name)))
(message "directory %s" (file-name-directory (or load-file-name (buffer-file-name))))

;;-- defvar is the correct way to declare global variables
;;-- setq is supposed to be use just to set variables and not create them.
(defvar required-packages
  '(
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
    color-theme-molokai                         ;; molokai theme
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
    company-tern                                ;; tern backend for company-mode
    ;;company-ycm                               ;; Emacs client for the YCM code-completion engine
    ;;;;;; auto-complete family                 ;;;;;;
    auto-complete                               ;; auto completion for gnu emacs
    auto-complete-distel                        ;; auto completion distel for erlang
    ac-haskell-process                          ;; haskell completion source for Emacs auto-complete
    auto-complete-nxml                          ;; auto-completion on nXml mode
    ac-cider                                    ;; clojure completion source
    auto-complete-clang                         ;; auto complete source for clang
    auto-complete-c-headers                     ;; auto-complete source for C/C++ header files
    ac-slime                                    ;; auto completion for slime
    fuzzy                                       ;; fuzzy matching utilities for GNU Emacs
    ;;;;;; some utilities                       ;;;;;;
    parent-mode                                 ;; get major mode's parent modes
    exec-path-from-shell                        ;; make Emacs use the $PATH set up by the user's shell
    ;;better-defaults                           ;; better defaults for Emacs
    ;;;;;; essential utilities                  ;;;;;;
    smartparens                                 ;; parenthesis management
    ;;;;;; documentation and help               ;;;;;;
    markdown-mode                               ;; markdown language support
                                        ; auctex                                    ;; AUCTEX and LATEX
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
    flycheck-mix                                ;; flycheck elixir mix support
    flycheck-clojure                            ;; flycheck clojure support
    flycheck-irony                              ;; flycheck c/c++ support via Irony
    flycheck-plantuml                           ;; flycheck for plantuml automatic syntax errors
    flycheck-rebar3                             ;; flycheck integration for rebar3 projects
    ;;;;;; spell checking                       ;;;;;;
    flyspell-lazy                               ;; improve Emacs flyspell responsiveness using idle timers
    helm-flyspell                               ;; helm extension for correcting words with flyspell
    ;;;;;; flymake family                       ;;;;;;
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
    ;;org-bullets                               ;; org mode with bullets
    ox-reveal                                   ;; for reveal.js presentations through org-mode
    org-tree-slide                              ;; presentations tools for org-mode
    ox-html5slide                               ;; export org-mode to html5 slide
    plantuml-mode                               ;; editing PlantUML sources
    latex-pretty-symbols                        ;; unicode display of characters
    org-download                                ;; image drag and drop for org-mode
    org-easy-img-insert                         ;; insert images from web
    ob-http                                     ;; make http request within org-mode babel
    epresent                                    ;; simple presentation mode for Emacs Org-mode
    htmlize                                     ;; convert buffer text and decorations to HTML
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
    ;;python-pylint                             ;; python linter
    pylint                                      ;; python linter
    pyvenv                                      ;; python virtual environment interface for Emacs
    py-autopep8                                 ;; integrate autopep8 into Emacs
    sphinx-doc                                  ;; sphinx style doc strings for python code
    pydoc-info                                  ;; better python support for info lookup
    ;;;;;; haskell programming modes            ;;;;;;
    haskell-mode                                ;; haskell language support
    company-ghc                                 ;; haskell company auto-completion
    company-ghci                                ;; a company backend for haskell
    company-cabal                               ;; cabal company support
    ghci-completion                             ;; completion for GHCi commands in inferior-haskell buffers
    shm                                         ;; structured haskell mode
    haskell-snippets                            ;; haskell language snippets
    hindent                                     ;; haskell code indenting
    hi2                                         ;; for haskell-indentation, 2nd try
    ghc                                         ;; haskell ghc
    ;;intero                                    ;; complete dev environment for haskell
    ;;;;;; erlang laguage support               ;;;;;;
    erlang                                      ;; erlang emacs plugin
    ;;ivy-erlang-complete                       ;; context sensitive completion for erlang
    edts                                        ;; Erlang Development Tool Suite
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
    go-guru                                     ;; go guru analysis tool
    go-eldoc                                    ;; eldoc for go-mode
    go-autocomplete                             ;; auto completion backend for go
    company-go                                  ;; Company complete for go
    golint                                      ;; lint for go source
    ;;;;;; c/c++ language support               ;;;;;;
    irony                                       ;; a c/c++ minor mode for Emacs powered by libclang
    irony-eldoc                                 ;; eldoc support in irony-mode
    clang-format                                ;; code formatting
    c-eldoc                                     ;; description of the arguments to C functions
    function-args                               ;; C++ completion for GNU Emacs
    cmake-project                               ;; Integrates CMake build process with Emacs
    ;;;;;; yasnippets package                   ;;;;;;
    yasnippet                                   ;; Yet another snippet extension
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
    buffer-move                                 ;; move buffer
    neotree                                     ;; a tree plugin like NerdTree for Vim
    dired-imenu                                 ;; imenu binding for dired mode
    ;;golden-ratio                              ;; auto re-size Emacs windows Tool golden ratio
    volatile-highlights                         ;; visual feedback on operations
    ;;;;;; essential utilities                  ;;;;;;
    highlight-symbol                            ;; automatic and manual symbol highlighting for Emacs
    xah-math-input                              ;; show math input symbols
    fiplr                                       ;; Emacs Fuzzy Find in Project Package
    ;;;;;; icon displays                        ;;;;;;
    ;;mode-icons                                ;; show icons for modes
    all-the-icons                               ;; package for showing various icons
    ;;;;;; editing and keyboard mappings        ;;;;;;
    key-chord                                   ;; map pairs of simultaneously pressed keys to commands
    diminish                                    ;; diminished modes are minor modes with no mode-line display
    multiple-cursors                            ;; multiple cursors for emacs
    ace-mc                                      ;; Add Multiple Cursors using Ace Jump
    iedit                                       ;; edit multiple regions simultaneously in a buffer or a region
    ;;;;;; web, html, java script and json      ;;;;;;
    web-mode                                    ;; major-mode for editing web templates
    js2-refactor                                ;; javascript refactoring library
    js2-highlight-vars                          ;; highlight variables
    tern                                        ;; JavaScript code analyzer
    tern-auto-complete                          ;; js tooling auto-complete
    ;;tern-context-coloring                     ;; adding scope coloring
    ;;tj-mode                                   ;; Highlight JavaScript with Tern
    js2-mode                                    ;; Improved JavaScript editing mode
    js3-mode                                    ;; chimeric fork of js2-mode and js-mode
    js-doc                                      ;; Insert JsDoc style comment easily
    jsfmt                                       ;; formatting, searching, and rewriting javascript
    ac-js2                                      ;; Javascript auto-completion
    json-mode                                   ;; major mode for json editing
    coffee-mode                                 ;; major mode for CoffeeScript
    company-web                                 ;; company backend for ac-html
    ;;indium                                    ;; JS Awesome Development Environment formerly jade
    ;;tidy                                      ;; interface to html tidy program
    ac-emmet                                    ;; auto-complete sources for emmet-mode
    emmet-mode                                  ;; emmet support for emacs
    web-beautify                                ;; Format HTML, CSS and JavaScript/JSON by js-beautify
    json-navigator                              ;; View and navigate JSON structures
    ;;;;;; window, text and file utilities      ;;;;;;
    popwin                                      ;; popup window manager
    popup                                       ;; show popup for flycheck
    clippy                                      ;; show tooltip with function documentation at point
    quick-peek                                  ;; inline window doc popup
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
    evil-indent-textobject
    evil-commentary
    evil-exchange
    evil-anzu
    evil-ediff
    evil-magit
    evil-avy
    evil-matchit
    evil-visualstar
    evil-numbers
    ;;;;;; clojure programming modes            ;;;;;;
    clojure-mode                                ;; Emacs support for clojure
    clojure-mode-extra-font-locking             ;; Extra font-locking for Clojure mode
    helm-cider                                  ;; helm interface for cider
    helm-clojuredocs                            ;; searching for help in clojuredocs.org with helm
    cider                                       ;; Clojure Interactive Development Environment that Rocks
    ;;;;;; You Complete Me  Completion          ;;;;;;
    ycmd                                        ;; emacs bindings to the ycmd completion server
    company-ycmd                                ;; company mode backend for ycmd
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
    ;;"javascript/node-ac"              ;; node-js auto-complete package
    "dircolors"                         ;; colored buffer
    "elisp/elisp-format"                ;; format elisp code
    "elisp/ps2pdf"                      ;; convert ps to pdf
    "psgml"                             ;; markup languages...
    "cpp-addon"                         ;; addon to cc-mode
    "flycheck-google-cpplint"           ;; google c++ style checker for flycheck
    "emacs-clang-complete-async/elisp"
    "ox-manuscript"
    )
  "Custom load paths that do not follow the normal vendor/elisp/module-name.el format.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-packages)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; aqua-packages.el ends here
