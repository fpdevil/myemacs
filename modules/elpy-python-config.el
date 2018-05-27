;;; package --- customize ELPY python configuration for Emacs
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;            ELPY Python 3 support
;;;
;;; Filename   : elpy-python-config.el
;;; Description: Python configuration for Emacs through ELPY
;;;              A full featured python ide and language support for Aquamacs
;;;
;;; Code:
;;;=============================================================================
(require 'elpy)                 ; Emacs Python Development Environment

;; --
;; visual clue on how the code is indented
(require-package 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start elpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Emacs Python Development Environment  ;;;;;;;;;;;;;;;;;;;;
;; elpy mode can be disabled or commented out if running 2 completion(s)      ;;
;; simultaneously is considered an overkill and right now the jedi setup      ;;
;; has been working more than satisfactorily... but leaving for now           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python auto completion and code checking
;; (elpy-enable t)
(after 'python (elpy-enable))

(setq elpy-modules '(
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-pyvenv
                     elpy-module-highlight-indentation  ;; might break older emacs
                     elpy-module-sane-defaults
                     elpy-module-yasnippet
                     ;; elpy-module-flymake
                     ))

;; use ipython3 if available (deprecated so commented out)
;;(if (executable-find "ipython3")
;;    (elpy-use-ipython (executable-find "ipython3")))

;; -- set the appropriate backend for Elpy
;; (setq elpy-rpc-backend "rope")
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command (executable-find "python3")
      elpy-rpc-python-path "/usr/local/lib/python3.6/site-packages"
      flycheck-python-flake8-executable (executable-find "flake8")
      python-check-command (executable-find "pyflakes")
      ;; python-check-command (concat user-emacs-directory "/flymake/pyflymake.py")
      python-environment-directory (concat user-emacs-directory "/.python-environments"))


;; -- Note
;; elpy auto-completion can be manually triggered using M-Tab (elpy-company-backend) as
;; per the documentation https://elpy.readthedocs.io/en/latest/ide.html#completion
;; make sure elpy python completions don't start automatically (if not needed)
(add-hook 'elpy-mode-hook
           (lambda ()
             (setq company-idle-delay nil)
             'elpy-mode-hook 'py-autopep8-enable-on-save))

;; flymake integration if flycheck is not present
(if (require 'flycheck nil t)
    (lambda ()
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))
  (add-hook 'elpy-mode-hook
            '(lambda ()
               (progn
                 (add-to-list 'flymake-err-line-patterns '("\\([^|]+\\)| \\([^:]+\\):\\([0-9]+\\)$" 2 3 nil 1))
                 (set (make-local-variable 'flymake-warning-predicate) "^.[^EF]")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end elpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'elpy-python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; elpy-python-config.el ends here
