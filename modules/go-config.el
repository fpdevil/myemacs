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
;;===========================================================================
(require 'golint)                 ; linter for the go code
(require 'go-guru)
(require 'go-eldoc)
(require-package 'flymake-go)
(require 'flymake-go)

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && golint && errcheck"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(after 'go-mode
  (add-hook 'go-mode-hook 'compilation-auto-quit-window)
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  (after "company-autoloads"
    (require-package 'company-go)
    (require 'company-go)
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go)))))

  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)

  ;; Load auto-complete
  (after 'auto-complete
    (require 'go-autocomplete)
    ;; (add-to-list 'ac-modes 'go-mode)
    (add-hook 'go-mode-hook 'auto-complete-mode))
  (setenv "GOPATH" (concat (getenv "HOME") "/sw/programming/gocode/go"))
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))

  (after 'go-eldoc
    (add-hook 'go-mode-hook 'go-eldoc-setup))

  (after 'go-guru
    (go-guru-hl-identifier-mode)))

(provide 'go-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; go-config.el ends here
