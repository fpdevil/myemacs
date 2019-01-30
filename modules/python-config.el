;;; package --- customize core python 3.x.x configuration for Emacs
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : python-config.el
;;; Description: Core Python 3.x.x configuration for Emacs.
;;;              A full featured python IDE and language support for Aquamacs
;;;
;;; Code:
;;;


;;**
;;** make Emacs aware of the version-dependent shebangs
(add-to-list 'auto-mode-alist '("\\.\\(py\\)$" . python-mode))
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

;;** python-mode package
(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python3"
  :config
  (defvar python-mode-initialized nil)
  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)
      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item))))))))))
  (add-hook 'python-mode-hook 'my-python-mode-hook))



;;**
;;**  settings for the python3 shell interpreter and indentation settings
(defun my-python-mode-config ()
  "Define settinigs for proper indentation and shell expressions."
  (setq default-tab-width 4
        indent-tabs-mode nil         ; autoconvert tabs to spaces
        python-indent 4
        python-indent-offset 4
        indent-level 4
        comment-inline-offset 2
        tab-width 4

        ;; python interpreter settings
        ;; if using ipython3 with extras
        python-shell-interpreter (executable-find "ipython3")
        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"

        ;; if using jupyter shell interpreter
        ;; python-shell-interpreter "jupyter"
        ;; python-shell-interpreter-args "console --simple-prompt"
        ;; python-shell-prompt-detect-failure-warning nil

        ;; additional shell options added
        ;;python-shell-prompt-regexp ">>> "
        ;;python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        ;;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "

        python-shell-completion-native-enable nil
        python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython" "python" "jupyter"))
        python-skeleton-autoinsert t

        py-python-command (executable-find "python3")
        py-shell-name "ipython"
        py-which-bufname "IPython"
        )

  (untabify (point-min) (point-max))
  (hs-minor-mode t)                     ; hs-minor-mode
  (auto-fill-mode 0)                    ; auto-fill-mode
  (whitespace-mode t)                   ; whitespace-mode
  ;;(hl-line-mode t)                    ; hl-line-mode
  (set (make-local-variable 'electric-indent-mode) nil))

(add-hook 'python-mode-hook 'my-python-mode-config)


;;**
;;** indentation through electric indent
(defun python-indent-setup ()
  "Set necessary python indentation."
  (unless (aqua/is-buffer-file-temp)
    ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
    ;; emacs 24.4 only
    (setq electric-indent-chars (delq ?: electric-indent-chars))))
;; (add-hook 'python-mode-hook 'python-indent-setup)

;;**
;;** handling white-spaces in python mode
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;**
;; if it is Emacs 25.1, there is a known bug that will cause run-python to
;; display a bunch of garbled characters in the python shell. The following
;; method can solve the problem.
(setenv "IPY_TEST_SIMPLE_PROMPT"  "1" )

;;**
;;** python checkers and python virtual environments
;; (setq python-check-command (executable-find "pyflakes"))
(setq python-check-command (concat user-emacs-directory "/flymake/pyflymake.py"))
(setq python-environment-directory (expand-file-name ".python-environments" user-emacs-directory))


;;**
;;** company mode - for auto completions
(after "company"
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0)
              (setq-local company-idle-delay 0.5))))


;;**
;; documentation and help -- enables eldoc for all
(eldoc-mode t)


;;**
;;** sphinx documentation generation for python
;;** move the cursor to some function/method definition and hit C-c M-d
(add-hook 'python-mode-hook
          (lambda ()
            (require 'sphinx-doc)
            (sphinx-doc-mode t)))

;;**
;;** python documentation under symbols pydoc-info
(require 'pydoc-info)
(info-lookup-add-help
   :mode 'python-mode
   :parse-rule 'pydoc-info-python-symbol-at-point
   :doc-spec
   '(("(python)Index" pydoc-info-lookup-transform-entry)
     ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry)))


;;**
;;** python virtual environment setup
(require 'virtualenvwrapper)           ;; python virtualenv wrapper
(venv-initialize-interactive-shells)   ;; if you want interactive shell support
(venv-initialize-eshell)               ;; if you want eshell support

;;
;; note that setting `venv-location` is not necessary if you use the default
;; location (`~/.virtualenvs`), or incase if the system environment variable
;; `WORKON_HOME` points to the right place
;;
;; (if (equal (getenv "WORKON_HOME") nil) (setenv "WORKON_HOME" "~/.virtualenvs"))
;; (setq pyvenv-activate (getenv "WORKON_HOME"))
;; (setq venv-location (expand-file-name "~/.virtualenvs/"))
;; (setq python-shell-virtualenv-root "~/.virtualenvs/")
;; (if (equal (getenv "VIRTUAL_ENV") nil) (setenv "VIRTUAL_ENV" "~/.virtualenvs"))
;; (if (equal (getenv "VIRTUALENVWRAPPER_PYTHON") nil)
;;   (setenv "VIRTUALENVWRAPPER_PYTHON" "/usr/local/bin/python3.7"))
;; (setq pyvenv-virtualenvwrapper-python (getenv "VIRTUALENVWRAPPER_PYTHON"))
;; (setq pyvenv-workon "default")
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python linting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  using FlyCheck with pyLint                                                ;;
;;  http://liuluheng.github.io/wiki/public_html/Python                        ;;
;;                                 /flycheck-pylint-emacs-with-python.html    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after "flycheck"
  ;;(add-hook 'python-mode-hook 'flycheck-mode)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-python-flake8-executable "~/.emacs.d/flymake/pycheckers"
                    flycheck-python-pylint-executable (executable-find "pylint")
                    ;;flycheck-pylintrc (concat (getenv "HOME") "/.pylintrc")
                    python-check-command (executable-find "pyflakes"))))

  ;;**
  ;;** flycheck for virtualenv
  (defun flycheck-virtualenv-setup ()
    "Setup Flycheck for the current virtualenv."
    (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup)
    (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))
  )


;;**
;;** SYNTAX CHECKING - (FlyCheck and FlyMake)
;;**   flymake handler for syntax-checking python source code
;;**   using wither pyflakes or flake8
;; (after "flymake"
;;   (require 'flymake-python-pyflakes)
;;   (add-hook 'python-mode-hook #'(lambda () (setq-local flymake-no-changes-timeout 10))) ;; default 0.5
;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;;   ;; disable fymake-cursor
;;   (setq-local flymake-cursor-auto-enable nil)
;;   ;; using flake8 for FlyMake
;;   (setq-local flymake-python-pyflakes-executable (executable-find "flake8")))


;; code checking via flymake
;; set code checker here from "pyflymake", "pyflakes"
;;(setq pycodechecker (concat user-emacs-directory "/flymake/pyflymake.py"))
(setq pycodechecker (concat user-emacs-directory "/flymake/pycheckers.py"))
(when (load "flymake" t)
  (require 'flymake-python-pyflakes)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))


;;**
;; == for autopep8 formatting and linting
;;    ignoring the below:
;; *- E501 - Try to make lines fit within --max-line-length characters.
;; *- W293 - Remove trailing whitespace on blank line.
;; *- W391 - Remove trailing blank lines.
;; *- W690 - Fix various deprecated code (via lib2to3).
;;    (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
;;
;;** code standardization with autopep8
(defcustom python-autopep8-path (executable-find "autopep8")
  "Python autopep8 executable path."
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


;;**
;;** formatting python code with yapf
;; (require 'py-yapf)              ; use yapf to beautify a Python buffer
;; discarding py-yapf in favor of yapfiy as py-yapf loses the kill ring when it runs
(require-package 'yapfify)
(add-hook 'python-mode-hook 'yapf-mode)


;;**
;;** helper functions
;;*** small helper to scrape text from shell output
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

;;{{{ grand unified debugger
;;**  Tell Python debugger (pdb) to use the current virtual environment
;;**  https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
(setq gud-pdb-command-name "python3 -m pdb ")
;;}}}


;;{{{ prettify symbols
;;**  when hovered show original
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;**
;;** symbols for concealing
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
;;}}}


;;{{{ for imenu integration
(defun my-merge-imenu ()
  "Imenu integration."
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))

(add-to-list
    'imenu-generic-expression
    '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
(setq imenu-create-index-function 'my-merge-imenu)
;;}}}


;;**
;;** pydoc with helm interface
(require-package 'helm-pydoc)


;;**
;;** add support for smartparens
(with-eval-after-load 'smartparens
  (require 'smartparens-python))

;; set PYTHONPATH, because we don't load .bashrc
(defun set-python-path-from-shell-PYTHONPATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))

(if window-system (set-python-path-from-shell-PYTHONPATH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; python-config.el ends here
