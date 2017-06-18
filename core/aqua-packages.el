;;; package --- list of packages to be installed by Aquamacs
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-packages.el
;;; description: This file contains new or missing packages to be installed
;;               during Emacs bootstrap. Any new packages to be installed
;;               for Emacs should be defined as a part of the section defined
;;               under required-packages. Any packages not available as ready
;;               packages should be in the vendor directory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code:
;;;

;;============================================================================;;
;;;; defvar is the correct way to declare global variables                  ;;;;
;;;; setq is supposed to be use just to set variables and not create them.  ;;;;
;;============================================================================;;
(defvar required-packages
  '(;;;;;; appearance and visual customizations ;;;;;;
    powerline                                   ;; powerline smart mode
    smart-mode-line                             ;; powerful and beautiful mode-line
    smart-mode-line-powerline-theme             ;; sml powerline theme
    ;delight                                    ;; customize mode names on modeline
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
    solarized-theme                             ;; bozhidaar solarized for emacs
    cyberpunk-theme                             ;; cyberpunk theme for emacs
    majapahit-theme                             ;; color theme with a dark and light versions
    zerodark-theme                              ;; dark medium-contrast theme
    flatui-theme                                ;; color theme based on flat colors
    spacemacs-theme                             ;; spacemacs light and dark themes
    ;;;;;; project setup and configuration      ;;;;;;
    projectile                                  ;; Project Interaction Library for Emacs
    helm-projectile                             ;; Helm UI for Projectile
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
    ;company-ycm                                ;; Emacs client for the YCM code-completion engine
    ;;;;;; auto-complete family                 ;;;;;;
    auto-complete                               ;; auto completion for gnu emacs
    auto-complete-distel                        ;; auto completion distel for erlang
    ac-haskell-process                          ;; haskell completion source for Emacs auto-complete
    auto-complete-nxml                          ;; auto-completion on nXml mode
    ac-cider                                    ;; clojure completion source
    auto-complete-clang                         ;; auto complete source for clang
    auto-complete-c-headers                     ;; auto-complete source for C/C++ header files
    ac-slime                                    ;; auto completion for slime
    ;;;;;; some utilities                       ;;;;;;
    parent-mode                                 ;; get major mode's parent modes
    exec-path-from-shell                        ;; make Emacs use the $PATH set up by the user's shell
    ;;;;;; essential utilities                  ;;;;;;
    smartparens                                 ;; parenthesis management
    evil-smartparens                            ;; evil integration for smartparens
    paredit                                     ;; minor mode for editing parentheses
    evil-paredit                                ;; evil extension for paredit
    ;;;;;; documentation and help               ;;;;;;
    markdown-mode                               ;; markdown language support
    ; auctex                                    ;; AUCTEX and LATEX
    graphviz-dot-mode                           ;; dotlanguage graphviz graphs (for erlang .dotfiles)
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
    popup                                       ;; show popup for flycheck
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
    org-plus-contrib                            ;; plus all contribs files
    org-ac                                      ;; auto completion for org
    org-bullets                                 ;; org mode with bullets
    ox-reveal                                   ;; for reveal.js presentations through org-mode
    org-tree-slide                              ;; presentations tools for org-mode
    ox-html5slide                               ;; export org-mode to html5 slide
    plantuml-mode                               ;; editing PlantUML sources
    latex-pretty-symbols                        ;; unicode display of characters
    org-download                                ;; image drag and drop for org-mode
    org-easy-img-insert                         ;; insert images from web
    ob-http                                     ;; make http request within org-mode babel
    epresent                                    ;; simple presentation mode for Emacs Org-mode
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
    sphinx-doc                                  ;; sphinx style doc strings for python code
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
    helm-describe-modes                         ;; Helm interface to Emacs’s describe-mode
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
    ace-mc                                      ;; Add Multiple Cursors using Ace Jump
    iedit                                       ;; edit multiple regions simultaneously in a buffer or a region
    ;;;;;; web, html, java script and json      ;;;;;;
    web-mode                                    ;; major-mode for editing web templates
    js2-refactor                                ;; javascript refactoring library
    js2-highlight-vars                          ;; highlight variables
    tern                                        ;; JavaScript code analyzer
    tern-auto-complete                          ;; js tooling auto-complete
    js2-mode                                    ;; Improved JavaScript editing mode
    js3-mode                                    ;; chimeric fork of js2-mode and js-mode
    js-doc                                      ;; Insert JsDoc style comment easily
    jsfmt                                       ;; formatting, searching, and rewriting javascript
    ac-js2                                      ;; Javascript auto-completion
    json-mode                                   ;; major mode for json editing
    coffee-mode                                 ;; major mode for CoffeeScript
    indium                                      ;; JS Awesome Development Environment formerly jade
    company-web                                 ;; company backend for ac-html
    ;tidy                                       ;; interface to html tidy program
    ac-emmet                                    ;; auto-complete sources for emmet-mode
    emmet-mode                                  ;; emmet support for emacs
    web-beautify                                ;; Format HTML, CSS and JavaScript/JSON by js-beautify
    json-navigator                              ;; View and navigate JSON structures
    ;;;;;; window, text and file utilities      ;;;;;;
    popwin                                      ;; popup window manager
    move-text                                   ;; move current line or region up or down
    switch-window                               ;; window switching, the visual way
    clippy                                      ;; show tooltip with function documentation at point
    imenu-list                                  ;; show the current buffer's imenu entries in a seperate buffer
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
    ;;;;;; vim emulation                        ;;;;;;
    evil                                        ;; Extensible Vi layer for Emacs.
    undo-tree                                   ;; Treat undo history as a tree (evil dependency)
    goto-chg                                    ;; goto last change (evil dependency)
    evil-leader                                 ;; let there be <leader>
    evil-surround                               ;; emulate surround.vim from Vim
    evil-mc                                     ;; multiple cursors for evil-mode
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
    ;;;;;; package installers                   ;;;;;;
    use-package                                 ;; use-package declaration
  )
  "A list of packages that will be installed if not present when firing Emacs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load packages from custom path                                         ;;;;
;;;; contains packages not in elpa/melpa/marmalade/gnu/org                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar custom-load-paths
  '(; "erlang/elisp"        ;; erlang lisp modules
    "xslide"                ;; xml and xslt syntax, customization's
    "xslt-process/lisp"     ;; xslt processor ide
    "javascript/node-ac"    ;; node-js auto-complete package
    )
  "Custom load paths that do not follow the normal vendor/elisp/module-name.el format.")

(message "Loaded the aquq-packages...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-packages)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-packages.el ends here