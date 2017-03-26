;;; package  --- html-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : html-config.el
;;; Description: Emacs configuration for html development support
;;;
;;; elisp code for customizing the web/html development settings
;;;===========================================================================
(require 'cl)
(require 'tidy)
(require 'emmet-mode)                                ; emmet support
(require 'company-web-html)                          ; load company mode html backend
(require 'company-web-jade)                          ; load company mode jade backend
(require 'company-web-slim)                          ; load company mode slim backend
;;;
;;; Code:
;;;
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
;; set up HTML mode to do indenting
(add-hook 'html-mode-hook
    (lambda ()
    (setq indent-line-function 'indent-relative)))

;; for html support through tidy
(add-hook 'html-mode-hook
          (lambda ()
            (tidy-build-menu html-mode-map)))

;; add emmet support
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; company web completions
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key
;; use company-mode with company-web-html in web-mode
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'html-config)
;;; html-config.el ends here
