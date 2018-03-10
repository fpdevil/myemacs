;;; package --- customize core python 3.x.x configuration for Emacs
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : python-config.el
;;; Description: Core Python 3.x.x configuration for Emacs.
;;;              A full featured python ide and language support for Aquamacs
;;;
;;; Code:
;;;=============================================================================

;; -- load all the syntax specific packages needed for python3
(require 'ring)                 ; browse the kill ring
(require 'epc)                  ; RPC stack for the Emacs Lisp
(require 'python-pylint)        ; minor mode for running pylint
(require 'py-yapf)              ; Use yapf to beautify a Python buffer
(require 'py-autopep8)          ; Integrate autopep8 into Emacs

;; -- python default shell completion disable
(setq-default python-shell-completion-native-enable nil)

;;; -- make Emacs aware of the version-dependent shebangs
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py$\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;;; -- proper python indentation settings
(add-hook 'python-mode-hook
          '(lambda ()
             ;; python common indentations
             (setq indent-tabs-mode nil
                   python-indent 4
                   python-indent-offset 4
                   indent-level 4
                   tab-width 4))
          (untabify (point-min) (point-max)))

;;; -- indentation through electric indent
(defun python-indent-setup ()
  "Set necessary python indentation."
  (unless (is-buffer-file-temp)
    ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))
(add-hook 'python-mode-hook 'python-indent-setup)

;;; -- rebind the Enter key to Ctrl+J for proper indentation with RET
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\r" 'newline-and-indent)))


;;; --  python3 shell interpreter
(setq python-shell-interpreter (executable-find "python3")
      ;; if extras are needed with ipython3
      ;; (setq python-shell-interpreter-args "--pylab")
      python-shell-interpreter-args (if (is-mac) "--matplotlib=osx --colors=Linux"
                                      (if (is-linux) "--gui=wx --matplotlib=wx --colors=Linux"))
      ;; additional shell options added
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      py-python-command (executable-find "python3"))

;; == below line specified for handling the args-out-of-range error in buffer
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-check-command (executable-find "pyflakes"))
(setq python-environment-directory (concat user-emacs-directory "/.python-environments"))

;; == handling white-spaces in python mode
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; -- set system path in the lisp for lib availability
;;;    == set PATH, because we don't load .bashrc
(setenv
 "PATH" (concat
   "$HOME/bin:"
   "/bin:"
   "/usr/bin:"
   "/sbin:"
   "/usr/sbin:"
   "/usr/local/bin:"
   "/usr/local/sbin"))

;;; -- set PYTHONPATH, because we don't load from .bashrc
(defun set-pypath-from-shell-pythonpath ()
  "Set the PYTHONPATH variable as its not pulled from .profile."
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))
(if (not (getenv "PYTHONPATH")) (setenv "PYTHONPATH" (executable-find "python3")))
(if window-system (set-pypath-from-shell-pythonpath))

;;; --
;;; -- auto completion with rope
(require-package 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-ropemacs)))

;;-----------------------------------------------------------------------------
;; documentation and help
;;-----------------------------------------------------------------------------
(eldoc-mode t)                              ;;; -- enable eldoc

;;; -- sphinx documentation generation for python
;;; -- move the cursor to some function/method definition and hit C-c M-d
(add-hook 'python-mode-hook
          (lambda ()
            (require 'sphinx-doc)
            (sphinx-doc-mode t)))

;;; -- python documentation under symbols pydoc-info
(require 'pydoc-info)
(info-lookup-add-help
   :mode 'python-mode
   :parse-rule 'pydoc-info-python-symbol-at-point
   :doc-spec
   '(("(python)Index" pydoc-info-lookup-transform-entry)
     ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry)))

;;-----------------------------------------------------------------------------
;;; -- python virtual environment setup
;;-----------------------------------------------------------------------------
(require 'virtualenvwrapper)           ;; python virtualenv wrapper
(venv-initialize-interactive-shells)   ;; if you want interactive shell support
(venv-initialize-eshell)               ;; if you want eshell support

;; note that setting `venv-location` is not necessary if you use the default
;; location (`~/.virtualenvs`), or incase if the system environment variable
;; `WORKON_HOME` points to the right place
(if (equal (getenv "WORKON_HOME") nil) (setenv "WORKON_HOME" "~/.virtualenvs"))
(setq pyvenv-activate (getenv "WORKON_HOME"))
(setq venv-location (expand-file-name "~/.virtualenvs/"))
(setq python-shell-virtualenv-root "~/.virtualenvs/")
(if (equal (getenv "VIRTUAL_ENV") nil) (setenv "VIRTUAL_ENV" "~/.virtualenvs"))
(if (equal (getenv "VIRTUALENVWRAPPER_PYTHON") nil)
  (setenv "VIRTUALENVWRAPPER_PYTHON" "/usr/local/bin/python3.6"))
(setq pyvenv-virtualenvwrapper-python (getenv "VIRTUALENVWRAPPER_PYTHON"))
(setq pyvenv-workon "default")

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
            (setq flycheck-python-flake8-executable (executable-find "flake8")
                  flycheck-pylintrc (concat (getenv "HOME") ".pylintrc"))))

;;------------------------------------------------------------------------------
;;; SYNTAX CHECKING - (FlyCheck and FlyMake)
;;------------------------------------------------------------------------------
;;; -- flymake handler for syntax-checking python source code
;;;    using pyflakes or flake8
(after "flymake"
  (require 'flymake-python-pyflakes)
  (add-hook 'python-mode-hook #'(lambda () (setq flymake-no-changes-timeout 10))) ;; default 0.5
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
  ;; using flake8 for FlyMake
  (setq flymake-python-pyflakes-executable (executable-find "flake8")))

;; flymake with pychecker script
(when (load "flymake" t)
  (defun flymake-pylint-init (&optional trigger-type)
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (options (when trigger-type (list "--trigger-type" trigger-type))))
      (list "~/.emacs.d/flymake/pyflymake.py" (append options (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py" flymake-pylint-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

(load-library "flymake-cursor")

;;------------------------------------------------------------------------------
;; == for autopep8 formatting and linting
;;    ignoring the below:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
;; (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
;;------------------------------------------------------------------------------
;;; -- code standardization with autopep8
(defcustom python-autopep8-path (executable-find "autopep8")
  "autopep8 executable path."
  :group 'python
  :type 'string)

(defun python-autopep8 ()
  "Automatically formats Python code to conform to the PEP 8 style guide.
$ autopep8 --in-place --aggressive --aggressive <filename>"
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command
     (format "%s --in-place --aggressive %s" python-autopep8-path
             (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(after 'py-autopep8
  (setq py-autopep8-options '("--ignore=W690"))
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;;; -- helper functions
;;;    == small helper to scrape text from shell output
(defun get-shell-output (cmd)
  "Scrape text from the shell output of the CMD."
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

(defun get-project-root (buf repo-file &optional init-file)
  "Just make use of the `vc-find-root' function to figure out the project root taking in BUF REPO-FILE and INIT-FILE.  Won't always work for some directory layouts."
  (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
         (project-root (vc-find-root buf-dir repo-file)))
    (if project-root
        (expand-file-name project-root)
      nil)))

;;------------------------------------------------------------------------------
;; grand unified debugger
;;------------------------------------------------------------------------------
;; Tell Python debugger (pdb) to use the current virtual environment
;; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
(setq gud-pdb-command-name "python3 -m pdb ")

;;------------------------------------------------------------------------------
;; prettify symbols
;;------------------------------------------------------------------------------
(setq prettify-symbols-unprettify-at-point 'right-edge) ;; when hovered show original

;; symbols for concealing
(add-hook 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(("def"    . "ƒ")
           ("class"  . "ℂ")
           ("and"    . "∧")
           ("or"     . "∨")
           ("not"    . "￢")
           ("in"     . "∈")
           ("not in" . "∉")
           ("return" . "η")
           ("for"    . "∀")
           ("!="     . "≠")
           ("=="     . "≡")
           (">="     . "≥")
           ("<="     . "≤")
           ("="      . "≃")))))

;;------------------------------------------------------------------------------
;;; -- Tree style source code viewer for Python buffer
;;------------------------------------------------------------------------------
(require-package 'jedi-direx)
(after "python"
  '(define-key python-mode-map "\C-c x" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)


;;------------------------------------------------------------------------------
;;; -- which function mode for displaying function names
;;------------------------------------------------------------------------------
; (after "which-func"
;   '(add-to-list 'which-func-modes 'python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; python-config.el ends here
