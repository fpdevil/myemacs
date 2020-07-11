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

;; load all the pre-requisite libraries
;; (require 'cl)
;; (require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;; Haskell settings for Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templates used from the below links.  Thanks to both                       ;;
;; https://github.com/serras/emacs-haskell-tutorial/                          ;;
;; https://github.com/chrisdone/emacs-haskell-config/                         ;;
;; -------------------------        references     -------------------------- ;;
;; http://www.mew.org/~kazu/proj/ghc-mod/en/                                  ;;
;; http://haskell.github.io/haskell-mode/manual/latest/                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; load necessary libraries for haskell and company (for auto completion)
(require 'haskell-mode)               ;; haskell editing mode for Emacs
(require 'hindent)                    ;; indentation for haskell program
(require 'haskell-font-lock)          ;; font lock mode for haskell
(require 'inf-haskell)                ;; haskell inferior mode
(require 'haskell-interactive-mode)   ;; haskell ghci support
(require 'haskell-process)            ;; haskell ghci repl support
(require 'ghc)                        ;; sub mode for haskell mode


(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** set up the required $PATH for the haskell and cabal environment
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;; Make Emacs look in to the Cabal directory for installed binaries
;; and then set the same into the classpath for ready access
(setenv "PATH" (shell-command-to-string "echo $PATH"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
(setq ghc-module-command "~/.local/bin/ghc-mod")

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**                        file mode associations
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(add-to-list 'auto-mode-alist '("\\.hs$"    . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$"   . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** haskell yasnippets
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(require-package 'haskell-snippets)

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**                emacs haskell-mode configuration setup
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
;; ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**               hoogle executable for documentation
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(defvar hoogle-url-base "http://haskell.org/hoogle/?q="
  "The base for the URL that will be used for web Hoogle lookups.")

(defvar hoogle-local-command  (concat (getenv "HOME") "/.local/bin/hoogle")
  "The name of the executable used for hoogle not found in $PATH (using `executable-find'), then a web lookup is used.")

(defvar hoogle-always-use-web nil
  "Set to non-nil to always use web lookups.")

(defvar hoogle-history nil
  "The history of what you've hoogled for.")

(defvar hoogle-result-count 15
  "How many results should be shown (when running locally.")

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** yasnippet integration
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(add-hook 'haskell-mode-hook 'yas-minor-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** haskell language server protocol
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(require-package 'lsp-haskell)
(require 'lsp-haskell)
(setq lsp-haskell-process-path-hie (executable-find "hie-wrapper"))
(setq lsp-haskell-process-args-hie '())
(setq lsp-haskell-set-completion-snippets-on t)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
(add-hook 'haskell-mode-hook #'lsp)



;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** haskell syntax checking, indentation and snippets
;;**                   use hi2 for haskell indentation
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;(require 'hi2)                        ;; indentation module for haskell mode
;;(add-hook 'haskell-mode-hook 'turn-on-hi2)

;;** using haskell-indentation, instead of SHM
;;   haskell-indentation and SHM are incompatible
;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;** with M-q, you can reformat the current declaration
(add-hook 'haskell-mode-hook #'hindent-mode)
(setq hindent-style "johan-tibell")

;;** disable electric indent mode for Haskell to prevent aggressive indenting
(add-hook 'haskell-mode-hook (lambda () (electric-indent-local-mode -1)))

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** [shm] - structured-haskell-mode configuration
;;**         using haskell-indentation instead of SHM
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
; (when (executable-find "structured-haskell-mode")
;   (require 'shm)
;   ;; using haskell-indentation, instead of SHM
;   ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;   (setq shm-program-name (executable-find "structured-haskell-mode"))
;   (set-face-background 'shm-current-face nil)
;   (set-face-background 'shm-quarantine-face nil))

;; ** customize colors while running shm (good colors for solarized) **
;; (set-face-background 'shm-current-face "#eee8d5")
;; (set-face-background 'shm-quarantine-face "lemonchiffon")
;; ** disabling the faces **

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** add F8 key combination for going to imports block
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** set or unset variables needed for customization
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(setq haskell-notify-p t
      ;; Remove annoying error popups
      haskell-interactive-popup-errors nil
      ;; for better import handling
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      ;; this might break flycheck checking
      haskell-stylish-on-save t
      haskell-process-type 'auto
      haskell-process-args-ghci '()
      haskell-tags-on-save nil
      haskell-process-log t
      haskell-process-reload-with-fbytecode nil
      haskell-process-use-presentation-mode t
      haskell-interactive-mode-include-file-name t
      haskell-interactive-mode-eval-pretty t
      haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s"
      shm-use-hdevtools nil
      shm-use-presentation-mode t
      shm-auto-insert-skeletons t
      shm-auto-insert-bangs t
      haskell-process-show-debug-tips t
      haskell-process-suggest-hoogle-imports t
      haskell-process-suggest-haskell-docs-imports t
      haskell-font-lock-symbols t
      )


;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**                  standard haskell module completions
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(setq haskell-complete-module-preferred
      '("Data.ByteString"
        "Data.ByteString.Lazy"
        "Data.Conduit"
        "Data.Function"
        "Data.List"
        "Data.Map"
        "Data.Maybe"
        "Data.Monoid"
        "Data.Text"
        "Data.Ord"))
(setq haskell-session-default-modules
      '("Control.Monad.Reader"
        "Data.Text"
        "Control.Monad.Logger"))
(setq haskell-interactive-mode-eval-mode 'haskell-mode)
(setq haskell-process-path-ghci "ghci-ng")
(setq haskell-process-args-ghci '("-ferror-spans"))
(setq haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;; Add key combinations for interactive haskell-mode
;; using ac-haskell-process-popup-doc to pop up documentation for the symbol at point
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c c")   'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
     (define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc)))

(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
     (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal)))


;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** ghc-mod configuration (initializer for ghc-mod)
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; with the below setting, we can watch the communication between Emacs
;; front-end and ghc-modi in the "*GHC Debug*" buffer.
(setq ghc-debug t)

;; if using any refactor tool like hare
;; (autoload 'hare-init "hare" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (hare-init)))

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;; company-ghc configuration (enable company-mode for auto-completion)
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(after "company"
  ;; company backend haskell-mode via ghc-mod
  (require 'company-ghc)
  ;; (add-hook 'haskell-mode-hook 'company-mode)
  ;; get auto completions in the ghci REPL
  (add-hook 'haskell-interactive-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq company-minimum-prefix-length 1
                    company-ghc-turn-on-autoscan t
                    ;; get completions from ghc-mod
                    company-ghc-show-info t
                    company-ghc-show-module t
                    company-ghc-component-prefix-match t
                    )))

  (add-hook 'haskell-mode-hook (lambda () (aqua/company-idle-delay 0.8 1 'haskell-mode)))
  (add-hook 'haskell-mode-hook (lambda () (aqua/company-backend-disable 'company-deabbrev 'haskell-mode)))
  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(company-ghc :with company-dabbrev-code))))


  ;; Default value of company-ghc-show-info is nil since when ghc-modi info is
  ;; called, ghc-mod pops up error if the current buffer contains error.
  ;; (setq company-ghc-show-info t)
  (setq company-ghc-hoogle-command (executable-find "hoogle"))

  ;; company-ghci configuration
  ;; company-ghci is a company backend that provides completions for the
  ;; haskell programming language by talking to a ghci process
  (require 'company-ghci)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-ghci)))
  )


;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;; auto-complete-mode for interacting with inferior haskell mode and popup
;; completion turn on when needed; haskell completion source for the
;; necessary auto-complete is provided by ac-haskell-process
;;
;; haskell completion source for Emacs auto-complete package
;; https://github.com/purcell/ac-haskell-process
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(after "auto-complete"

  ;; (add-hook 'haskell-mode-hook
  ;;           (lambda () (auto-complete-mode 1)))
  (setq ac-delay 1.0) ;; change auto-complete value to not conflict with company

  (require 'ac-haskell-process)
  '(progn

     ;; to get a variable in scope
     ;; (auto-complete-mode)
     (ac-define-source ghc-mod
       '((depends ghc)
         (candidates . (ghc-select-completion-symbol))
         (symbol . "s")
         (cache)))

     (defun my-ac-haskell-mode ()
       "Set auto-complete candidates for haskell."
       (setq ac-sources '(ac-source-words-in-same-mode-buffers
                          ac-source-dictionary
                          ac-source-ghc-mod)))

     (defun my-haskell-ac-init ()
       "Init auto-complete sources for haskell-mode when you open a file."
       (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
         (setq ac-sources '(ac-source-words-in-same-mode-buffers
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
     (add-to-list 'ac-modes 'haskell-interactive-mode)
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

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**  Completion for GHCi commands in inferior-haskell buffers
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(require 'ghci-completion)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** custom set variables for haskell
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(setq haskell-language-extensions '())
(setq haskell-process-type 'ghci)
(setq haskell-process-path-ghci (executable-find "ghci"))
(setq haskell-process-path-stack (executable-find "stack"))
(setq haskell-hoogle-command (executable-find "hoogle"))
(setq haskell-process-use-ghci t)

;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
;;** laod the Haskell helpers...
;;== == == == == == == == == == == == == == == == == == == == == == == == == == ==
(defcustom haskell-helper-config nil "Haskell helpers.")

(setq haskell-helper-config
      (expand-file-name "haskell-helper-config.el" module-dir))
(when (file-exists-p haskell-helper-config)
  (load haskell-helper-config 'noerror))



(provide 'haskell-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; haskell-config.el ends here
