;;; package --- elixir configuration settings
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename: elixir-config.el
;;; Description: A major mode elixir language support in Emacs
;;;
;;; elisp code for elixir language support and handling
;;===========================================================================
(require 'elixir-mode)         ;; this is installed as a alchemist dependency
(require 'alchemist)           ;; alchemist for elixir major mode
(require 'flycheck-elixir)     ;; flycheck checker for Elixir files
(require 'flycheck-mix)
(require 'ac-alchemist)        ;; auto-complete source of alchemist
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using elixir-mode and alchemist                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'elixir-mode-hook 'alchemist-mode)
(add-to-list 'elixir-mode-hook 'company-mode)
(setq alchemist-mix-command "/usr/local/bin/mix"
      alchemist-iex-program-name "/usr/local/bin/iex"
      alchemist-execute-command "/usr/local/bin/elixir"
      alchemist-compile-command "/usr/local/bin/elixirc"
      alchemist-key-command-prefix (kbd "C-c a"))

;; add support for Elixir and mix to flycheck.
(flycheck-mix-setup)
(add-hook 'elixir-mode-hook 'flycheck-mode)

(add-hook 'elixir-mode-hook
          (lambda()
            (company-mode)
            (alchemist-mode)
            (add-to-list (make-local-variable 'company-backends)
              '(alchemist-company
                :with company-yasnippet))))

;; add hook for indentation on save
(add-hook 'elixir-mode-hook
          (lambda()
            (add-hook 'before-save-hook
                      (lambda()
                        (indent-whole-buffer)))))

;; auto-complete
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;; company auto completion
;; (require 'company)
;; (add-hook 'eval-after-load 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'elixir-config)

;;; elixir-config.el ends here
