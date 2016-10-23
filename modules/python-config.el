;;; package --- customize python configuration for Emacs
;;;
;;; name: python-config.el
;;; description: python configuration for emacs
;;;
;;; Commentary:
;;             python language support for Emacs
;--------------------------------------------------------------------

;;
; load the required packages
;;
(require 'cl)
(require 'cl-lib)
;;
; load company mode for jedi
;;
(require 'company-jedi)

;;; Code:

;;
; python virtualenv wrapper
;;
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location (expand-file-name "~/.virtualenvs/"))


;;
; python jedi
; python development with auto-completion and intelli-sense
; for running inferior process when loading major mode python
;;
(defun run-python-once ()
  "Python specific hook settings."
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python (python-shell-parse-command)))

(add-hook 'python-mode-hook 'run-python-once)

;;
; python jedi ide
;;
(require 'jedi)

;;
; Start auto-complete and jedi for refactoring
;;
(setq flymake-log-level 3)
;; Hook up to autocomplete
; (add-to-list ’ac-sources ’ac-source-jedi-direct)
(require 'ring)
(require 'epc)
(autoload 'jedi:setup "jedi" nil t)
;; enable python-mode
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi-config:set-python-executable "/usr/local/bin/python3")
;(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
; hack to never show in-function call automatically
(setq jedi:get-in-function-call-delay 300)
(setq jedi:server-command (list "/usr/local/bin/python3" jedi:server-script))
(setq jedi:tooltip-method '(pos-tip))
(jedi-mode 1)
(setq jedi:environment-root "env")
; (setq jedi:environment-virtualenv
;   (append python-environment-virtualenv
;     '(' "--python" "/usr/local/bin/python3")))

(defun jedi-config:setup-keys ()
  "Custom keyboard mapping for jedi."
       (local-set-key (kbd "M-.") 'jedi:goto-definition)
       (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
       (local-set-key (kbd "M-?") 'jedi:show-doc)
       (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)


;;
; company backend for Python jedi
;;
(defun my-python-hooks()
    (eval-after-load "company"
        '(progn
            (unless (member 'company-jedi (car company-backends))
                (setq comp-back (car company-backends))
                (push 'company-jedi comp-back)
                (setq company-backends (list comp-back)))
            )))
(add-hook 'python-mode-hook 'my-python-hooks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; shell interpreter
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq py-python-command "/usr/local/bin/python3")
; (setq python-shell-interpreter "/usr/local/bin/ipython3")
(setq python-shell-interpreter "ipython3"
      ;; if extras are needed with ipython3
      ; (setq python-shell-interpreter-args "--pylab")
      python-shell-interpreter-args "-i"
      ;; additional shell options added
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code  "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
      ;; python command
      py-python-command "/usr/local/bin/python3")
;; below for handling the args-out-of-range error in buffer
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-check-command "/usr/local/bin/pyflakes")
(setq python-environment-directory "~/.emacs.d/.python-environments")
;; handling whitespaces in python mode
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;
; load python jedi-core
;;
(require 'jedi-core)

;;
; system path in the lisp
; set PATH, because we don't load .bashrc
;;
(setenv
 "PATH" (concat
   "$HOME/bin:"
   "/bin:"
   "/usr/bin:"
   "/sbin:"
   "/usr/sbin:"
   "/usr/local/bin:"
   "/usr/local/sbin"))

;;
; Set PYTHONPATH, because we don't load .bashrc
;;
(setenv "PYTHONPATH" "/usr/local/lib/python3.5/site-packages:")


;;
; python linting
;;
(require 'python-pylint)
(load "python-pylint")


;;
; yapf to beautify a Python buffer
;;
(require 'py-yapf)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; elpy
; Emacs Python Development Environment
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(setq
  elpy-rpc-backend "jedi"
  elpy-rpc-python-command "/usr/local/bin/python3"
  elpy-rpc-python-path "/usr/local/lib/python3.5/site-packages"
  flycheck-python-flake8-executable "/usr/local/bin/flake8"
  python-check-command "/usr/local/bin/pyflakes"
  python-environment-directory "~/.emacs.d/.python-environments")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; enable eldoc
;;
(eldoc-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'python-config)
;;; python-config ends here
