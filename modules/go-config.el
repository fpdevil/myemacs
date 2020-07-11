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

;; snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(defun my-go-mode-hook ()
  "GO customizations as needed."
  (setq gofmt-command "goimports")                  ; use goimports instead of go-fmt
  (add-hook 'before-save-hook 'gofmt-before-save)   ; call gofmt before saving
  (if (not (string-match "go" compile-command))     ; customize compile command to run go build
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && golint && errcheck"))
                                        ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

;; (add-hook 'go-mode-hook 'compilation-auto-quit-window)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require-package 'go-complete)
(add-hook 'completion-at-point-functions 'go-complete-at-point)

(after 'go-mode
  (after "company"
    (require-package 'company-go)
    (require 'company-go)
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go)))))

  ;; Load auto-complete
  (after "auto-complete"
    (require 'go-autocomplete)
    ;; (add-to-list 'ac-modes 'go-mode)
    (add-hook 'go-mode-hook 'auto-complete-mode)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'flycheck-mode)

(setenv "GOPATH" (concat (getenv "HOME") "/sw/programming/go"))
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))

(after 'go-eldoc (add-hook 'go-mode-hook 'go-eldoc-setup))

;; highlight identifiers
(after 'go-guru (go-guru-hl-identifier-mode))

(provide 'go-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; go-config.el ends here
