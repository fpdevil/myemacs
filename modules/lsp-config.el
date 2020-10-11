;;; package --- language server protocol configuration settings
;;;
;;; Commentary:
;;; Filename   : lsp-config.el
;;;              EGLOT and LSP - Client for LSP Servers
;;; Code:
;;;


(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-treemacs)
(require 'lsp-ivy)
(require 'company-lsp)
(require 'dap-mode)
(require 'dap-ui)


(defun /aqua/lsp-init ()
  "Aqua lsp mode init."
  (lsp-flycheck-enable)
  (flycheck-inline-mode -1)
  (when lsp-enable-symbol-highlighting
    (add-hook #'lsp-on-idle-hook #'lsp--document-highlight nil t)
    (lsp--info "Symbol highlighting enabled in current buffer."))
  (lsp-enable-which-key-integration))


(after 'lsp-mode
  (setq lsp-auto-configure t
        lsp-prefer-flymake nil
        lsp-eldoc-render-all t
        lsp-eldoc-enable-hover t
        lsp-auto-guess-root t
        lsp-log-io t
        lsp-enable-snippet nil
        lsp-enable-folding nil
        lsp-enable-snippet nil
        lsp-enable-completion-at-point nil
        lsp-enable-links nil
        lsp-restart 'auto-restart
        lsp-print-performance t
        lsp-client-packages '(lsp-clients)
        lsp-session-file (expand-file-name (locate-user-emacs-file "cache/.lsp-session-v1")))

  ;;Optional - provides fancier overlays.
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-flycheck-list-position 'bottom
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top)
  (when (memq window-system '(mac ns))
    (setq lsp-ui-doc-use-childframe nil))
)


;; Debug Adapter Procol
(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
(add-hook 'lsp-mode-hook 'dap-mode)
(add-hook 'lsp-mode-hook 'dap-ui-mode)

;; company mode is a standard completion package that works well with lsp-mode.
; ;company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(after 'company
    (push 'company-lsp company-backends))


(provide 'lsp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; lsp-config.el ends here
