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
;;; Updated    : 16 Mar 2018
;;;



;; loop through each and load the configured custom packages
;; each configuration file has a format of name-config.el
;; (mapc 'load (directory-files module-dir nil "^[^#].*el$"))

(add-to-list 'load-path module-dir)             ;; load the modules dir

;; == debug messages
(message "loading package repos settings %s"  (or load-file-name (buffer-file-name)))
(message "loading from directory %s" (file-name-directory (or load-file-name (buffer-file-name))))


;; == load all the required
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up,
;; it has to runs those regexps against the filename.
(let* ((file-name-handler-alist nil))

  ;; bootstrap the first one...
  (require 'bootstrap-config)                     ; initial settings and some miscellaneous stuff
  (require 'exec-path-config)                     ; setup $PATH
  (require 'utils-config)                         ; utilities like window configurations, imenu-list etc.
  
  (require 'helm-config)                          ; emacs helm fuzzy
  (require 'ido-config)                           ; emacs ido navigation
  (require 'ivy-config)                           ; emacs ivy navigation

  (require 'yas-config)                           ; emacs snippets
  (require 'ac-complete-config)                   ; auto completion ac
  (require 'company-config)                       ; company auto completion
  (require 'ycm-config)                           ; YouCompleteMe Intelligent Completion
  (require 'lsp-config)                           ; language server protocol

  (require 'sp-config)                            ; smart parentheses
  (require 'paredit-config)                       ; para management
  (require 'evil-config)                          ; emacs vim emulation
  (require 'ibuffer-config)                       ; iBuffer
  (require 'undo-tree-config)                     ; manage undo history
  (require 'which-func-config)                    ; display current function name under scope
  
  (require 'dired-config)                         ; for dired*
  (require 'bookmarks-config)                     ; manage bookmarks
  (require 'indent-config)                        ; manage indentation
  (require 'fiplr-config)                         ; Fuzzy Find in Project Package
  (require 'quick-peek-config)                    ; Quick-peek inline-window library
  (require 'popwin-config)                        ; text pop up for completion hints
  (require 'beacon-config)                        ; visually show where am i
  (require 'multiple-cursors-config)              ; manage multiple cursors
  (require 'vregex-config)                        ; regex support
  (require 'dumbjump-config)                      ; jump to function definition
  (require 'neotree-config)                       ; neotree for file explorer
  (require 'iedit-config)                         ; edit multiple regions
  (require 'hippie-config)                        ; hippie expansions

  ;;**  color themes switching section
  ;; comment below line to setup color theme in another way
  (if (or (display-graphic-p) (string-match-p "256color"(getenv "TERM"))) (require 'themes-config))
  (require 'mode-line-config)                       ; emacs modeline (sml + airline)

  (require 'flycheck-config)                      ; flycheck syntax checking
  (require 'flyspell-config)                      ; spell check
  (require 'flymake-config)                       ; flymake syntax checking

  (require 'python-config)                        ; python 3.x.x programming core settings
  (require 'jedi-python-config)                   ; python 3.x.x auto completion through JEDI
  (require 'elpy-python-config)                   ; python 3.x.x auto completion through ELPY

  (require 'elisp-config)                         ; ELisp programming language
  (require 'javascript-config)                    ; JS code completion, syntax checking
  (require 'web-config)                           ; for html and web markup langugae support
  (require 'coffee-config)                        ; Coffee Script syntax/auto-complete

  (require 'cpp-helper-config)                    ; c/c++ common auxilliary settings
  (require 'cpp-irony-config)                     ; c++ code completion using irony
  (require 'cpp-config)                           ; c++ & c programming language support
  (require 'ctags-config)                         ; ctags generation helper

  (require 'go-config)                            ; Go programming language
  (require 'rust-config)                          ; Rust programming language
  (require 'clojure-config)                       ; Clojure programming language
  (require 'erlang-config)                        ; Erlang programming language
  (require 'hs-config)                            ; Haskell
  (require 'scala-config)                         ; Scala programming language
  (require 'psgml-config)                         ; markup language support
  (require 'xslide-config)                        ; xsl ide settings
  (require 'nxml-config)                          ; for xml syntax validation
  (require 'shell-config)                         ; for shell scripting support

  (require 'org-config)                           ; for org mode
  (require 'tex-config)                           ; TeX mode
  (require 'plantuml-config)                      ; org mode diagrams
  (require 'slides-config)                        ; org mode presentations
  (require 'markdown-config)                      ; for Markdown files *.md

  (require 'weather-config)                       ; get wether details
  (require 'gitgutter-config)                     ; vcs management
  (require 'magit-config)                         ; Git Management
  (require 'hydra-config)                         ; Hydra configuration
  (require 'rbow-config)                          ; rainbow colors for parentheses
  (require 'highlight-symbol-config)              ; highlight current symbol
  (require 'rainbow-delims-config)                ; rainbow colors for brackets
  (require 'rbow-identifiers-config)              ; rainbow colors for variables
  (require 'clr-identifiers-config)               ; color identifiers mode
  (require 'whichkey-config)                      ; get details of key bindings
  (require 'guidekey-config)                      ; get details of all emacs key mappings
  (require 'diminish-config)                      ; diminish minor modes in mode line


  ;; -- unused configurations --
  ;;(require 'elixir-config)                        ; ELIXIR programming language
  ;;(require 'semantic-config)                      ; emacs semantic completion (C/C++)
  ;;(require 'auto-insert-config)                   ; auto insert tags and snippets
  ;;(require 'projectile-config)                    ; project management
  ;;(require 'ipythonnb-config)                     ; ipython notebook configuration
  ;;(require 'ecb-config)                           ; emacs code browser
  ;;(require 'html-config)                          ; for html files
  ;;(require 'spell-config)                         ; not used
  ;;(require 'fringe-config)                        ; thin strip down the left and/or right edge
  ;;(require 'gud-config)                           ; grand unified debugger settings
  ;;(require 'xslt-process-config)                  ; for xslt syntax and completion
  ;; -- unused configurations --

  )

;;; -- now load personal elisp files if any from personal directory
(when (file-exists-p personal-dir)
  (message "Loading personal configuration files in %s..." personal-dir)
  (mapc 'load (directory-files personal-dir 't "^[^#\.].*el$")))


(provide 'aqua-package-repos)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-package-repos.el ends here
