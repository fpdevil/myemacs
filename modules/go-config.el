;;; package --- go language configuration settings
;;;
;;; Commentary:
;;; Filename   : go-config.el
;;;
;;; Description: A major mode go language support in Emacs
;;; optional install libraries
;;; $ go get -u golang.org/x/tools/cmd/...
;;; $ go get -u github.com/rogpeppe/godef/...
;;; $ go get -u github.com/nsf/gocode
;;; $ go get -u golang.org/x/tools/cmd/goimports
;;; $ go get -u golang.org/x/tools/cmd/guru
;;; $ go get -u github.com/dougm/goflymake
;;;
;;; elisp code for go language support and handling
;;;
;;; Code:
;;;


(require 'golint)
(require 'go-guru)
(require 'go-eldoc)
(require-package 'flymake-go)
(require-package 'go-complete)

(setenv "GOPATH" (concat (getenv "HOME") "/sw/programming/go"))
(add-to-list
 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))

;; snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; eglot lsp integration
;; (require 'eglot)
;; (add-hook 'go-mode-hook 'eglot-ensure)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(go-mode . ("gopls"))))

(defun /aqua/init-lsp-go ()
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(defun /aqua/setup-go-mode-compile ()
  "Customize compile command to run go build."
  (if (not (string-match "go" compile-command))
      ;; customize compile command to run go build
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && golint && errcheck")))


(defun /aqua/go-mode-hook ()
  "GO customizations as needed."
  (setq-local tab-width 4)
  ;; use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; call gofmt before saving
  (add-hook 'go-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook 'gofmt-before-save nil
                         ;; Buffer local hook.
                         t)))
  (add-hook 'go-mode-hook #'/aqua/setup-go-mode-compile)
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(after "company"
  (require-package 'company-go)
  (require 'company-go)
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go)))))

;; Load auto-complete
(after "auto-complete"
  (require 'go-autocomplete)
  (add-to-list 'ac-modes 'go-mode)
  (add-hook 'go-mode-hook 'auto-complete-mode))

;; hooks
(add-hook 'go-mode-hook (lambda () (ycmd-mode)))        ; use ycmd/lsp
(add-hook 'go-mode-hook #'/aqua/go-mode-hook)
(add-hook 'completion-at-point-functions 'go-complete-at-point)
;; (add-hook 'go-mode-hook 'compilatin-auto-quit-window)
;; (add-hook 'go-mode-hook #'/aqua/init-lsp-go)       ; use lsp-go

(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'go-guru-hl-identifier-mode) ;; highlight identifiers

(provide 'go-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; go-config.el ends here
