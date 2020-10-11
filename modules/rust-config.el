;;; package --- rust-config.el configuration settings for rust
;;;
;;; Commentary:
;;;
;;; Filename   : rust-config.el
;;; Description: RUST Language configuration
;;;
;;;
;;; Code:
;;;

(require 'racer)
(require 'rust-mode)
(require 'cargo)
(require 'ac-racer)
(require 'company-racer)
(require 'flycheck-rust)
(require 'toml-mode)
;; to use rustic-mode even if rust-mode also installed
;; (require 'rustic)

(defun /aqua/racer-ac-mode-hook ()
  "Setup AutoComplete mode."
  (ac-racer-setup))

;; package does this by default ;; set racer rust source path environment variable
;; run -> rustup component add rust-src
;; set the below value in bashrc ->
;; export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library
(setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
;; (setq racer-rust-src-path
;;       (concat (shell-command-to-string "rustc --print sysroot") "/lib/rustlib/src/rust/library"))

;; add cargo-minor-mode to your rust-mode-hook
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook 'racer-mode)      ; activate racer when rust-mode starts
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)

;; using ycmd for rust completion
;; (add-hook 'rust-mode-hook (lambda () (ycmd-mode)))

;; comppany completion mode
(after 'company
  (require 'company-racer)
  (add-hook 'rust-mode-hook  #'company-mode)
  (add-hook 'racer-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-racer))
                (company-mode)))
  (setq-local company-minimum-prefix-length 1))

;; auto completion mode
(after 'auto-complete
  (add-hook 'racer-mode-hook #'/aqua/racer-ac-mode-hook))

;; flycheck integration
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; for formatting rust buffers on save using rustfmt
(add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'rust-mode)
              (rust-format-buffer))))

(provide 'rust-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rust-config.el ends here
