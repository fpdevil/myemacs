;;------------------------------------------------------------------------------
;; EGLOT and LSP - Client for LSP Servers
;;------------------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :defer t
  :hook
  (go-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-prefer-flymake nil)
  :config

  ;; change nil to 't to enable logging of packets between emacs and the LS
  ;; this was invaluable for debugging communication with the MS Python Language Server
  ;; and comparing this with what vs.code is doing
  (setq lsp-print-io nil)

  ;; lsp-ui gives us the blue documentation boxes and the sidebar info
  (use-package lsp-ui
    :ensure t
    :requires lsp-mode flycheck
    :config
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-position 'top
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25
          lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
    :requires company
    :config
    (push 'company-lsp company-backends))

  (require 'lsp-clients)
  ;; (add-hook 'typescript-mode-hook 'lsp)
  ;; (add-hook 'js2-mode-hook 'lsp)
  ;; (add-hook 'php-mode 'lsp)
  ;; (add-hook 'css-mode 'lsp)
  ;; (add-hook 'ruby-mode 'lsp)
  ;; (add-hook 'web-mode 'lsp)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  )

;;(use-package company-lsp :ensure t :defer t :hook (lsp-mode))

;; (setq lsp-language-id-configuration '(
;;                       (java-mode . "java")
;; 				      ;; (python-mode . "python")
;; 				      ;; (gfm-view-mode . "markdown")
;; 				      ;; (rust-mode . "rust")
;; 				      ;; (ruby-mode . "ruby")
;; 				      ;; (css-mode . "css")
;; 				      ;; (xml-mode . "xml")
;; 				      ;; (c-mode . "c")
;; 				      ;; (c++-mode . "cpp")
;; 				      ;; (objc-mode . "objective-c")
;; 				      ;; (web-mode . "html")
;; 				      ;; (html-mode . "html")
;; 				      ;; (sgml-mode . "html")
;; 				      ;; (mhtml-mode . "html")
;; 				      ;; (go-mode . "go")
;; 				      ;; (haskell-mode . "haskell")
;; 				      ;; (php-mode . "php")
;; 				      ;; (json-mode . "json")
;; 				      ;; (js2-mode . "javascript")
;; 				      ;;(typescript-mode . "typescript")
;; 				      ))

;; LSP debugging
(setq lsp-print-io t)
(setq lsp-trace t)
(setq lsp-print-performance t)

(use-package dap-mode
  :after lsp-mode
  :defer t
  ;;:hook (lsp-mode . dap-mode)
  :config
    (dap-mode t)
    (dap-ui-mode t))

(use-package eglot
  :pin melpa-stable
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("hie" "--lsp" "--debug")))
  ;; (optional) Automatically start metals for haskell files.
  :hook (haskell-mode . eglot-ensure))

(provide 'lsp-config)
