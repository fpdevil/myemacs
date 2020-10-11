;;; package --- haskell configuration
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; configuration file for haskell mode
;;; Filename: haskell-config.el
;;; Description: A major mode haskell language support in Emacs
;;; ref: http://haskell.github.io/haskell-mode/manual/latest/index.html#Top
;;
;; elisp code for haskell language support and handling
;;;
;;; Code:
;;;


;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** set up the required $PATH for the haskell and cabal environment
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(setenv "PATH" (shell-command-to-string "echo $PATH"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
;; (setq ghc-module-command "~/.local/bin/ghc-mod")
;; (add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))

(add-to-list 'auto-mode-alist '("\\.hs\\'"  . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hs$"    . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$"   . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))


(defun haskell-do-info (&optional cPos cEnd)
  "Bring up REPL and do :info on symbol at poinnt.
If interactive and region active or CPOS and CEND are non-nil, use that region."
  (interactive "r")
  (let ((symbol (if (and (and cPos cEnd)
                         (or (region-active-p)
                             (not (called-interactively-p 'interactive))))
                    (buffer-substring-no-properties cPos cEnd)
                  (thing-at-point 'symbol))))
    (haskell-interactive-switch)
    (haskell-interactive-mode-run-expr (format ":info %s" symbol)))
  (goto-char (point-max))
  (haskell-interactive-switch-back))


(use-package haskell-mode
  :ensure t
  :config
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
  (setq haskell-process-suggest-add-package nil
        haskell-ask-also-kill-buffers nil
        haskell-stylish-on-save t
        haskell-process-auto-import-loaded-modules t
        haskell-process-suggest-remove-import-lines  t
        haskell-process-log t
        haskell-interactive-popup-errors nil
        haskell-doc-show-global-types nil
        haskell-doc-show-reserved     t
        haskell-doc-show-prelude      nil ;;t
        haskell-doc-show-strategy     t
        haskell-doc-show-user-defined t
        haskell-doc-chop-off-context  nil ;; t
        haskell-doc-chop-off-fctname  nil
        haskell-doc-prettify-types    nil)
  (add-hook 'haskell-mode-hook (lambda () (whitespace-toggle-options 'tabs)))
  :config
  (define-key haskell-mode-map (kbd "M-,") #'pop-tag-mark)
  (define-key haskell-mode-map (kbd "C-,") #'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.") #'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "M-RET") (defun haskell-mode-open-line (n-todo)
                                               (interactive "P")
                                               (insert "\n")
                                               (backward-char)))
  (define-key haskell-mode-map (kbd "C-c C-c") #'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-z") #'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c t") #'haskell-mode-show-type-at)
  (define-key haskell-mode-map (kbd "C-c v c") #'haskell-cabal-visit-file)
  (define-key haskell-mode-map (kbd "C-c <") #'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-c >") #'haskell-move-nested-right)

  :custom
  (add-to-list 'completion-at-point-functions     'haskell-process-completions-at-point)
  (haskell-align-imports-pad-after-name            t) ;; nil)
  (haskell-ask-also-kill-buffers                   nil)
  (haskell-completions-complete-operators          nil)
  (haskell-compile-cabal-build-command            "stack build -j 7 --fast --ghc-options=\"-j +RTS -A32M -RTS\"")
  ;; (haskell-hoogle-url                             "https://hoogle.haskell.org/?hoogle=%s")
  (haskell-hoogle-command                          "hoogle --color -l --numbers --count=10")
  (haskell-indentation-electric-flag               t)
  (haskell-interactive-mode-eval-mode             'haskell-mode)
  (haskell-interactive-mode-hide-multi-line-errors nil)
  (haskell-interactive-mode-include-file-name      nil)
  (haskell-interactive-mode-scroll-to-bottom       t)
  (haskell-interactive-types-for-show-ambiguous    t) ;; check
  (haskell-interactive-popup-errors                nil)


  (haskell-mode-stylish-haskell-path              "stylish-haskell")
  (haskell-notify-p                                t)
  (haskell-process-args-cabal-repl                '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (haskell-process-args-cabal-new-repl            '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (haskell-process-args-ghci                      '("-ferror-spans" "-fshow-loaded-modules"))
  (haskell-process-args-stack-ghci                '("--ghci-options=-ferror-spans"
                                                    "--ghci-options=-fshow-loaded-modules"
                                                    "--ghci-options=-fno-diagnostics-show-caret"
                                                    "--no-build"
                                                    "--no-load"
                                                    ))
  (haskell-interactive-set-+c                     nil) ;; testing
  (haskell-process-auto-import-loaded-modules      t) ;;nil)
  (haskell-process-do-cabal-format-string         ":!cd %s && %s")
  (haskell-process-load-or-reload-prompt           nil)
  (haskell-process-log                             t)
  (haskell-process-path-ghci                       "stack exec ghci")
  (haskell-process-reload-with-fbytecode           t) ;; nil)
  (haskell-process-suggest-haskell-docs-imports    t)
  (haskell-process-suggest-hoogle-imports          t)
  (haskell-process-suggest-hayoo-imports           nil)
  (haskell-process-suggest-remove-import-lines     t)

  (haskell-process-suggest-add-package             t)
  (haskell-process-type                           'stack-ghci)
  (haskell-process-use-presentation-mode           t)
  (haskell-process-show-debug-tips nil)
  (haskell-stylish-on-save                         nil) ;; twice undo-tree saving
  (haskell-tags-on-save                            nil) ;; using projectile with codex every 30 secs
  (haskell-complete-module-preferred
   '("ClassyPrelude" "Data.Conduit" "Data.Function" "Data.List" "Data.Map"))
  (haskell-language-extensions
   '("ConstraintKinds"
     "BangPatterns"
     "ConstraintKinds"
     "DataKinds"
     "DeriveGeneric"
     "EmptyDataDecls"
     "ExistentialQuantification"
     "FlexibleContexts"
     "FlexibleInstances"
     "GADTs"
     "GeneralizedNewtypeDeriving"
     "KindSignatures"
     "LambdaCase"
     "MultiParamTypeClasses"
     "MultiWayIf"
     "NoImplicitPrelude"
     "NoMonomorphismRestriction"
     "OverloadedStrings"
     "PartialTypeSignatures"
     "PatternGuards"
     "QuasiQuotes"
     "RankNTypes"
     "RecordWildCards"
     "ScopedTypeVariables"
     "StandaloneDeriving"
     "TemplateHaskell"
     "TupleSections"
     "TypeApplications"
     "TypeFamilies"
     "TypeOperators"
     "TypeSynonymInstances"
     "ViewPatterns"
     ))
  )


(use-package ghci-completion
  :config
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

;; Prepare auto completion with company
(use-package company-ghci
  :ensure t
  :config
  (eval-after-load 'company-mode (add-to-list 'company-backends 'company-ghci)))


;; Prepare flyCheck
(use-package flycheck-haskell :ensure t
	:bind (:map haskell-mode-map
              ("M-n" . 'flycheck-next-error)
              ("M-p" . 'flycheck-previous-error))
  :init (add-hook 'haskell-mode-hook 'flycheck-haskell-setup))


(setq completion-at-point-functions
      (append '(haskell-process-completions-at-point)
              completion-at-point-functions))

;; Prepare Snippets
(use-package haskell-snippets :ensure t)

(use-package hlint-refactor
  :defer t
  :diminish hlint-refactor-mode
  :init (add-hook 'haskell-mode-hook #'hlint-refactor-mode))

;; Setup auto-completion with company
(use-package company-cabal
  :ensure t
  :config
  (eval-after-load 'company-mode (add-to-list 'company-backends 'company-cabal)))

;; Setup mode for hamlet, julius and lucius templates.
(use-package shakespeare-mode
  :ensure t
  :bind (:map shakespeare-mode-map
              ("C-<"   . 'haskell-move-nested-left)
              ("C->"   . 'haskell-move-nested-right)
              ;; ("<tab>" . 'shakespeare-hamlet-mode-indent-line)
              )
  :init
  )

;; Custom variables for Haskell
(setq tags-case-fold-search t) ;; tags operations case-sensitive

;; (use-package lsp-haskell
;;   :ensure t
;;   :init
;;   (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
;;   (add-hook 'haskell-mode-hook #'lsp)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   (setq lsp-haskell-process-path-hie (executable-find "hie-wrapper"))
;;   (setq lsp-haskell-process-args-hie '())
;;   (setq lsp-haskell-set-completion-snippets-on t))

(use-package hindent
  :defer t
  :after haskell-mode
  :bind (:map hindent-mode-map
              ("C-c d" . hindent-reformat-decl))
  :config
  (progn
    (setq hindent-style nil)
    (add-hook 'haskell-mode-hook 'hindent-mode))
  :init
  (when (locate-library "hindent")
    (add-hook 'haskell-mode-hook #'hindent-mode))
  (setq hindent-style "gibiansky"))

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;; auto-complete-mode for interacting with inferior haskell mode and popup
;; completion turn on when needed; haskell completion source for the
;; necessary auto-complete is provided by ac-haskell-process
;;
;; haskell completion source for Emacs auto-complete package
;; https://github.com/purcell/ac-haskell-process
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(after 'auto-complete
  ;; (add-hook 'haskell-mode-hook (lambda () (auto-complete-mode 1)))
  ;; change auto-complete value to not conflict with company
  (setq-local ac-delay 1.0)
  (require 'ac-haskell-process)
  '(progn

     ;; to get a variable in scope
     ;; (auto-complete-mode)
     ;; (ac-define-source ghc-mod
     ;;   '((depends ghc)
     ;;     (candidates . (ghc-select-completion-symbol))
     ;;     (symbol . "s")
     ;;     (cache)))

     (defun my-ac-haskell-mode ()
       "Set auto-complete candidates for haskell."
       (setq-local ac-sources '(ac-source-words-in-same-mode-buffers
                          ac-source-dictionary
                          ac-source-ghc-mod)))

     (defun my-haskell-ac-init ()
       "Init auto-complete sources for haskell-mode when you open a file."
       (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
         (setq-local ac-sources '(ac-source-words-in-same-mode-buffers
                                  ac-source-dictionary
                                  ac-source-ghc-mod))))

     ;; for auto-complete with TAB in repl
     (defun set-auto-complete-as-completion-at-point-function ()
       "Completion table lookup for thing at point."
       (add-to-list 'completion-at-point-functions 'auto-complete))

     (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
     (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
     (add-to-list 'ac-modes 'haskell-interactive-mode)
     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'haskell-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'haskell-mode-hook 'my-ac-haskell-mode)
     (add-hook 'find-file-hook 'my-haskell-ac-init)
     )
  )


;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;; haskell standard module imports
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(setq haskell-import-mapping
      '(("Data.Text" . "import qualified Data.Text as T
                        import Data.Text (Text)")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT")
        ("Data.ByteString" . "import qualified Data.ByteString as S
                              import Data.ByteString (ByteString)")
        ("Data.ByteString.Char8" . "import qualified Data.ByteString.Char8 as S8
                                    import Data.ByteString (ByteString)")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L")
        ("Data.ByteString.Lazy.Char8" . "import qualified Data.ByteString.Lazy.Char8 as L8")
        ("Data.Map" . "import qualified Data.Map.Strict as M
                       import Data.Map.Strict (Map)")
        ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
                              import Data.Map.Strict (Map)")
        ("Data.Set" . "import qualified Data.Set as S")
        ("Data.Vector" . "import qualified Data.Vector as V
                          import Data.Vector (Vector)")
        ("Data.Vector.Storable" . "import qualified Data.Vector.Storable as SV
                                   import Data.Vector (Vector)")
        ("Data.Conduit.List" . "import qualified Data.Conduit.List as CL")
        ("Data.Conduit.Binary" . "import qualified Data.Conduit.Binary as CB")))


(setq haskell-language-extensions '())


;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; flymake handler for checking Haskell source code with hlint.
;; flymake-hlint is handler for checking Haskell source with hlint
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(after "flymake"
  (setq flymake-run-in-place nil)
  (require 'flymake-hlint)
  (add-hook 'haskell-mode-hook 'flymake-hlint-load)

  ;; haskell flymake configuration
  (when (load "flymake" t)
    (defun flymake-hslint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "~/.emacs.d/flycheck/hslint" (list local-file))))

    (add-to-list 'flymake-allowed-file-name-masks '("\\.hs$\\'" flymake-hslint-init))
    (add-to-list 'flymake-allowed-file-name-masks '("\\.lhs$\\'" flymake-hslint-init))))

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**  DOC Strings
;;*** auto insert haskell module header when a new file is created
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(require 'skeleton)
(require 'autoinsert)

;;** A Haskell module definition template (inserted first time when opened)
(define-skeleton haskell-module-skeleton
  "Haskell source module header."
  "Brief description (can be left blank for defaults):"
  "-----------------------------------------------------------------------------\n"
  "-- \|\n"
  "-- Module      : " (setq v1 (or (haskell-guess-module-name) "Main"))"\n"
  "-- Copyright   : " (haskell-cabal-guess-setting "copyright") | (concat "© " user-full-name)"\n"
  "--\n"
  "-- License     : " (haskell-cabal-guess-setting "license") | "BSD-style (see the file LICENSE)""\n"
  "-- Author      : " (user-full-name)"\n"
  "-- Maintainer  : " (haskell-cabal-guess-setting "maintainer") | user-mail-address"\n"
  "-- Description : " str | (concat "The \\\"" v1 "\\\" module")"\n"
  "--\n"
  "   "_"\n"
  "\n"
  "--\n"
  "module " v1 " where\n\n")

;; un-comment the below line to insert module docstring as defined in haskell-module-skeleton
;; (add-to-list 'auto-insert-alist '("\\.hs\\|.lhs\\'" . haskell-module-skeleton))

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** a simple module level document string insertion
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(defun haskell-auto-insert-module-template ()
  "Insert a simple module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module ")
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)))

;; in order to use the above to insert doc string, uncomment below
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** indentation settings
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; A function used to change the behaviour of return.  If you press it at
;; the start of a line, it will just move the code down a line.  If you
;; press it anywhere else, you get automatic indentation.
(defun haskell-ret()
  "Return"
  (interactive)
  (if (bolp) (newline) (newline-and-indent)))
(add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "RET") 'haskell-ret)))

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Get Haskell indentation which mirrors what I'm used to from Vim
(defun haskell-indent-setup ()
  "Setup variables for editing Haskell files."
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (number-sequence 0 120 4))
  (setq indent-line-function 'tab-to-tab-stop)

  ;; Backspace: delete spaces up until a tab stop
  (defvar my-offset 4 "My indentation offset. ")
  (defun backspace-whitespace-to-tab-stop ()
    "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
    (interactive)
    (let ((movement (% (current-column) my-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char)))))
  (local-set-key (kbd "DEL") 'backspace-whitespace-to-tab-stop))
(add-hook 'haskell-mode-hook 'haskell-indent-setup)


;;------------------------------------------------------------------------------
;;** alignment rules for haskell
;;------------------------------------------------------------------------------
(use-package align
  :bind ("M-[" . align)
  :ensure nil
  :config
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))

(add-hook 'haskell-mode-hook 'eldoc-mode)

;;** disable electric indent mode for Haskell to prevent aggressive indenting
(add-hook 'haskell-mode-hook (lambda () (electric-indent-local-mode -1)))

;;------------------------------------------------------------------------------
;; DANTE
;;------------------------------------------------------------------------------
(require-package 'dante)
(add-hook 'haskell-mode-hook 'dante-mode)

;;------------------------------------------------------------------------------
;; client for LSP Servers
;;------------------------------------------------------------------------------
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint))))

;;------------------------------------------------------------------------------
;; inserting LANGUAGE pragmas
;;------------------------------------------------------------------------------
(defun insert-language-pragma (pragma)
  "Insert a LANGUAGE pragma at the top of the file."
  (interactive "SPragma: ")  ; ask for the name of the pragma
  (let ((string  (format "{-# LANGUAGE %s #-}" pragma)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat string "\n")))))


;;------------------------------------------------------------------------------
;; stylish-on-save toggling on|off
;;------------------------------------------------------------------------------
(defun toggle-stylish-on-save ()
  "Toggle haskell-stylish-on-save"
  (interactive)
  (setq haskell-stylish-on-save (if (eq haskell-stylish-on-save t) nil t))
  (message "Stylish-on-save is now %s." (if (eq haskell-stylish-on-save t) "on" "off")))

(provide 'hs-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; hs-config.el ends here
