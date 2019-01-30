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
;;;

;;**
;;** visual clue on how the code is indented
(require-package 'highlight-indentation)
(require 'highlight-indentation)

(defun hl-py-mode ()
  "Highlight indentation for python."
  (lambda ()
    (add-hook 'prog-mode-hook 'highlight-indentation-mode)
    (when (> (length (defined-colors))
           16)
      (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode))))

(add-hook 'python-mode-hook 'hl-py-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start elpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Emacs Python Development Environment  ;;;;;;;;;;;;;;;;;;;;
;; elpy mode can be disabled or commented out if running 2 completion(s)      ;;
;; simultaneously is considered an overkill and right now the jedi setup      ;;
;; has been working more than satisfactorily... but leaving for now           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;**
;;** elpy-use-ipython is deprecated
(require-package 'elpy)
(when (require 'elpy nil t)
  (elpy-enable))

(setq elpy-modules
      '(
        elpy-module-company
        elpy-module-eldoc
        elpy-module-pyvenv
        elpy-module-highlight-indentation  ;; might break older emacs
        elpy-module-sane-defaults
        elpy-module-yasnippet
        ))

;;**
;;**  set appropriate backend for ELPY (rope | jedi) and also rpc settings
(setq python-shell-interpreter (executable-find "ipython3"))
(setq elpy-rpc-backend "rope"
      elpy-rpc-python-command "/usr/local/bin/python3"
      elpy-rpc-python-path "/usr/local/lib/python3.7/site-packages")


;;**
;;** Note
;; elpy auto-completion can be manually triggered using M-Tab (elpy-company-backend) as
;; per the documentation https://elpy.readthedocs.io/en/latest/ide.html#completion
;; make sure elpy python completions don't start automatically (if not needed)
;; to use jedi and elpy for the company-backends
;; (after "company"
;;   (progn
;;     '(add-hook 'python-mode-hook 'config/company-jedi-backend)))
(defun config/company-jedi-backend()
  "Configure company-backends for company-jedi and company-yasnippet."
  '(progn
     (unless (member 'company-jedi (car company-backends))
       (setq comp-back (car company-backends))
       (push '(company-jedi :with company-yasnippet) comp-back)
       (setq company-backends (list comp-back)))))

(setq elpy-company-add-completion-from-shell t)


;;**
;;** enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;**
;;** use flycheck not flymake with elpy
;;** flymake integration if flycheck is not present
(if (require 'flycheck nil t)
    (lambda ()
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (set elpy-syntax-check-command "/usr/local/bin/pylint")
      (add-hook 'elpy-mode-hook 'flycheck-mode))
  (add-hook 'elpy-mode-hook
            '(lambda ()
               (progn
                 (add-to-list 'flymake-err-line-patterns '("\\([^|]+\\)| \\([^:]+\\):\\([0-9]+\\)$" 2 3 nil 1))
                 (set (make-local-variable 'flymake-warning-predicate) "^.[^EF]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end elpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'elpy-python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; elpy-python-config.el ends here
