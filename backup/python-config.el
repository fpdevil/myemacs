;;; package --- customize python configuration for Emacs
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;            Python 3 support
;;;
;;; Filename   : python-config.el
;;; Description: Python configuration for Emacs
;;;              A full featured python ide and language support for Aquamacs
;;; https://github.com/wernerandrew/jedi-starter/blob/master/jedi-starter.el
;;;=============================================================================

;; load all the pre-requisites first
(require 'cl)
(require 'cl-lib)

;; load all the syntax specific packages needed for python3
(require 'jedi)                 ; a Python auto-completion for Emacs
(require 'company-jedi)         ; company-mode completion back-end for Python JEDI
(require 'jedi-core)            ; Common code of jedi.el and company-jedi.el
(require 'ring)                 ; browse the kill ring
(require 'epc)                  ; RPC stack for the Emacs Lisp
(require 'elpy)                 ; Emacs Python Development Environment
(require 'python-pylint)        ; minor mode for running pylint
(require 'py-yapf)              ; Use yapf to beautify a Python buffer
(require 'py-autopep8)          ; Integrate autopep8 into Emacs
(require 'virtualenvwrapper)    ; python virtualenv wrapper

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python default shell completion disable                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-shell-completion-native-enable nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make Emacs aware of the version-dependent shebangs                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rebind Enter to Ctrl+j for proper indentation                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
          (lambda ()
             (define-key python-mode-map "\r" 'newline-and-indent)))

;; for autopep8 formatting and linting
;; ignoring the below:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
;; (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(setq py-autopep8-options '("--ignore=W690"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  python3 shell interpreter                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq python-shell-interpreter "/usr/local/bin/ipython3"
      ;; if extras are needed with ipython3
      ; (setq python-shell-interpreter-args "--pylab")
      python-shell-interpreter-args (if (is-mac) "--matplotlib=osx --colors=Linux"
                                      (if (is-linux) "--gui=wx --matplotlib=wx --colors=Linux"))
      ;; additional shell options added
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      py-python-command "/usr/local/bin/python3")
;; below line specified for handling the args-out-of-range error in buffer
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-check-command "/usr/local/bin/pyflakes")
(setq python-environment-directory (concat (getenv "HOME") "/.emacs.d/.python-environments"))
;; handling white-spaces in python mode
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system path in the lisp                                                   ;;
;; set PATH, because we don't load .bashrc                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setenv
;;  "PATH" (concat
;;    "$HOME/bin:"
;;    "/bin:"
;;    "/usr/bin:"
;;    "/sbin:"
;;    "/usr/sbin:"
;;    "/usr/local/bin:"
;;    "/usr/local/sbin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set PYTHONPATH, because we don't load from .bashrc                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-pypath-from-shell-pythonpath ()
  "Set the PYTHONPATH variable as its not pulled from .profile."
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))

(if window-system (set-pypath-from-shell-pythonpath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python virtual environment setup                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)             ;; if you want eshell support
(setq venv-location
      (expand-file-name (concat (getenv "HOME") "/.virtualenvs/")))

(setq python-shell-virtualenv-root (concat (getenv "HOME") "/.virtualenvs/")
      pyvenv-virtualenvwrapper-python "/usr/local/bin/python3")

;; python indentation settings
(add-hook 'python-mode-hook
          '(lambda ()
             ;; python common
             (setq indent-tabs-mode nil
                   python-indent 4
                   python-indent-offset 4
                   indent-level 4
                   tab-width 4))
          (untabify (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start elpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Emacs Python Development Environment  ;;;;;;;;;;;;;;;;;;;;
;; elpy mode can be disabled or commented out if running 2 completion(s)      ;;
;; simultaneously is considered an overkill and right now the jedi setup      ;;
;; has been working more than satisfactorily... but leaving for now           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python completion and code checking
;; (elpy-enable)
(defun python-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    (if (fboundp 'elpy-enable) (elpy-enable) (elpy-mode 1))
    ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))
(add-hook 'python-mode-hook 'python-mode-hook-setup)
(when (require 'elpy nil t)
  (elpy-enable t))

(setq elpy-modules '(
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-pyvenv
                     elpy-module-highlight-indentation ;breaks older emacs
                     elpy-module-sane-defaults
                     ))

;; use ipython3 if available
;; (elpy-use-ipython)
(if (executable-find "/usr/local/bin/ipython3")
    (elpy-use-ipython "/usr/local/bin/ipython3"))

(setq elpy-rpc-backend "jedi"
      elpy-rpc-python-command "/usr/local/bin/python3"
      elpy-rpc-python-path "/usr/local/lib/python3.6/site-packages"
      flycheck-python-flake8-executable "/usr/local/bin/flake8"
      python-check-command "/usr/local/bin/pyflakes"
      python-environment-directory "~/.emacs.d/.python-environments")


;; Note
;; elpy auto-completion can be manually triggerred using M-Tab (elpy-company-backend) as
;; per the documentation https://elpy.readthedocs.io/en/latest/ide.html#completion
;; make sure elpy python completions don't start automatically (if not needed)
(add-hook 'elpy-mode-hook
           (lambda ()
              (setq company-idle-delay nil)
              'elpy-mode-hook 'py-autopep8-enable-on-save))

;; visual clue on how the code is indented
;; (require 'highlight-indentation)
;; (add-hook 'python-mode-hook 'highlight-indentation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end elpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python jedi setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reference@http://tkf.github.io/emacs-jedi/latest/#                         ;;
;; python development with auto-completion and intelli-sense                  ;;
;; for running inferior process when loading major mode python                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-python-once ()
  "Python specific hook settings."
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python (python-shell-parse-command)))
(add-hook 'python-mode-hook 'run-python-once)

;; global jedi config vars
(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")

(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")

;; enable python-mode with jedi
(add-hook 'python-mode-hook 'jedi:setup)

;; load and setup jedi
(eval-after-load "jedi"
  '(progn
     (setq jedi:server-command
           (list "/usr/local/bin/python3" jedi:server-script))))

;; jedi logging with debug
(setq jedi:server-args
      '("--log-level" "DEBUG"
        "--log-traceback"))

;; Enable Jedi setup on mode start
(add-hook 'python-mode-hook 'jedi:setup)

(add-hook 'python-mode-hook
          '(lambda ()
             (setq jedi:complete-on-dot t
                   ;; jedi:setup-keys t
                   ;; hack to never show in-function call automatically
                   ;; jedi:get-in-function-call-delay 0.2
                   jedi:get-in-function-call-delay 0
                   jedi:tooltip-method '(pos-tip popup)
                   jedi:tooltip-show '(pos-tip popup)
                   jedi:doc-mode 'rst-mode
                   jedi:environment-root "jedi"
                   jedi:environment-virtualenv (append python-environment-virtualenv
                                                      '("--python" "/usr/local/bin/python3")))))

;; jedi keyboard mappings
(defun jedi-config:setup-keys ()
  "Custom keyboard mapping for jedi."
   (local-set-key (kbd "M-TAB") 'jedi:complete)
   (local-set-key (kbd "M-.") 'jedi:goto-definition)
   (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
   (local-set-key (kbd "M-?") 'jedi:show-doc)
   (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company backend(s) setup for python jedi                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-python-hooks()
    (eval-after-load "company"
        '(progn
            (unless (member 'company-jedi (car company-backends))
                (setq comp-back (car company-backends))
                (push 'company-jedi comp-back)
                (setq company-backends (list comp-back)))
            )))
(add-hook 'python-mode-hook 'my-python-hooks)

(with-eval-after-load 'python
  (remove-hook 'python-mode-hook #'python-setup-shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python linting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  using flycheck with pylint                                                ;;
;;  http://liuluheng.github.io/wiki/public_html/Python                        ;;
;;                                 /flycheck-pylint-emacs-with-python.html    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flycheck-python-setup ()
  "Flycheck python lint support."
  (flycheck-mode))

(add-hook 'python-mode-hook #'flycheck-python-setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq flycheck-python-flake8-executable "/usr/local/bin/flake8"
                  flycheck-pylintrc (concat (getenv "HOME") ".pylintrc"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake handler for syntax-checking Python source code using               ;;
;; pyflakes or flake8                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; using flake8 for flycheck
;; (setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")
;; set flymake log level
(setq flymake-log-level 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pylint/pyflakes integration through flymake                                ;;
;; Configure flymake for Python                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    "Flymake lint with pylint."
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      ;; (list "pep8" (list "--repeat" local-file))
      (list "epylint" (list local-file))))

  (defun flymake-pyflakes-init ()
    "FlyMake lint with pyflakes."
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/local/bin/pyflakes" (list local-file)))))


(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Set as a minor mode for Python
(add-hook 'python-mode-hook
          (lambda ()
            (unless (eq buffer-file-name nil) (flymake-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To avoid having to mouse hover for the error message, these functions      ;;
;; make flymake error messages appear in the minibuffer                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer."
  (interactive)
  (require 'cl)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))
(add-hook 'post-command-hook 'show-fly-err-at-point)

;; show message in the mini buffer
(defun flymake-show-help ()
  "Display help in minibuffer"
  (interactive)
  (when (get-char-property (point) 'flymake-overlay-p)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "FlyMake %s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yapf mode for beautifying a python buffer                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable eldoc                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eldoc-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sphinx documentation for python                                            ;;
;; move the cursor to some function/method definition and hit C-c M-d         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
          (lambda ()
            (require 'sphinx-doc)
            (sphinx-doc-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which function mode for displaying function names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'python-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopep8 functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun p8 ()
  "Apply autopep8 to the current region or buffer."
  (interactive)
  (unless (region-active-p)
    (mark-whole-buffer))
  (shell-command-on-region
    (region-beginning) (region-end)       ;; begin and end of a region/buffer
    "autopep8 -"                          ;; autopep8 command and flags
    (current-buffer)                      ;; the output buffer
    t                                     ;; do replace?
    "*autopep8 errors*"                   ;; name for error buffer
    t)                                    ;; show the error buffer?
  (goto-char (region-end))                ;; delete trailing newlines
  (re-search-backward "\n+" nil t)
  (replace-match "" nil t))

(defun p8-ediff ()
  "Compare the current buffer to the output of autopep8 using ediff."
  (interactive)
  (let ((p8-output
          (get-buffer-create (format "* %s autopep8 *" (buffer-name)))))
    (shell-command-on-region
      (point-min) (point-max)             ;; begin and end of region/buffer
      "autopep8 -"                        ;; autopep8 command and flags
      p8-output                           ;; the output buffer
      nil                                 ;; do replace?
      "*autopep8 errors*"                 ;; name of error buffer
      t)                                  ;; show the error buffer?
    (ediff-buffers (current-buffer) p8-output)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto completion with rope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-ropemacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small helper to scrape text from shell output
(defun get-shell-output (cmd)
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

(defun get-project-root (buf repo-file &optional init-file)
  "Just uses the vc-find-root function to figure out the project root.
       Won't always work for some directory layouts."
  (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
         (project-root (vc-find-root buf-dir repo-file)))
    (if project-root
        (expand-file-name project-root)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; python-config.el ends here
