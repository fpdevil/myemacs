;; File              : python-config.el
;; Author            : Sampath Singamsetty <Singamsetty.Sampat@gmail.com>
;; Date              : 11.04.2019
;; Last Modified Date: 17.04.2019
;; Last Modified By  : Sampath Singamsetty <Singamsetty.Sampat@gmail.com>
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
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;;** show tabs in Python; adapted from http://www.emacswiki.org/emacs/ShowWhiteSpace
(defface nasty-tab-face
  '((t (:background "red"))) "Used for tabs.")
(defvar nasty-tab-keywords
  '(("\t" . 'nasty-tab-face)))
(add-hook 'python-mode-hook
          (lambda () (font-lock-add-keywords nil nasty-tab-keywords)))

;;**
;;**  settings for the python3 shell interpreter and indentation settings
(defun aqua/python-indentation-settings ()
  "Set indentation settings for python."
  "Define settinigs for proper indentation and shell expressions."
  (setq default-tab-width 4
        indent-tabs-mode nil		; autoconvert tabs to spaces
        python-indent 4
        python-indent-offset 4
        indent-level 4
        comment-inline-offset 2
        tab-width 4
        python-indent-guess-indent-offset-verbose nil))

;; set PYTHONPATH, because we don't load .bashrc
(defun set-python-path-from-shell-PYTHONPATH ()
  "Set PYTHONPATH."
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))
;; (if window-system (set-python-path-from-shell-PYTHONPATH))


(defun aqua/python-interpreter-settings ()
  "Set requireed interpreter settings with ipython3 extras."
  (when (executable-find "ipython3")
    (setq python-shell-interpreter "ipython3"
          ;;python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-interpreter-args "--simple-prompt -i"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
          python-shell-icompletion-setup-code "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(defun aqua/python-shell-completion-settings ()
  "Set python shell completions."
  (setq python-shell-completion-native-enable nil
        python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython" "python" "jupyter"))
        python-skeleton-autoinsert nil
        py-python-command (executable-find "python3")
        py-shell-name "ipython"
        py-which-bufname "IPython"))


;;**
;;** handling white-spaces in python mode
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;** custom settings
(untabify (point-min) (point-max))
(auto-fill-mode 0)                    ; auto-fill-mode
(whitespace-mode t)                   ; whitespace-mode
(set (make-local-variable 'electric-indent-mode) nil)

;;** add settings for python
(add-hook 'python-mode-hook 'aqua/python-indentation-settings)
(add-hook 'python-mode-hook 'aqua/python-interpreter-settings)
(add-hook 'python-mode-hook 'aqua/python-shell-completion-settings)


;; set python environment
(setq python-environment-directory
      (expand-file-name ".python-environments" user-emacs-directory))

;;**
;; if it is Emacs 25.1, there is a known bug that will cause run-python to
;; display a bunch of garbled characters in the python shell. The following
;; method can solve the problem.
(setenv "IPY_TEST_SIMPLE_PROMPT"  "1" )



;;**
;;** sphinx documentation generation for python
;;** move the cursor to some function/method definition and hit C-c M-d
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;;**
;;** python documentation under symbols pydoc-info
; (require 'pydoc-info)
; (pydoc-info-add-help '("python" "sphinx"))
; (info-lookup-add-help
;  :mode 'python-mode
;  :parse-rule 'pydoc-info-python-symbol-at-point
;  :doc-spec
;  '(("(python)Index" pydoc-info-lookup-transform-entry)
;    ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry)))


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
                    python-check-command (executable-find "pyflakes")))))

;;**
;;** SYNTAX CHECKING - (FlyCheck and FlyMake)
;;**   flymake handler for syntax-checking python source code
;;**   using wither pyflakes or flake8
;;     code checking via flymake
;;     set code checker here from "pyflymake", "pyflakes"
;; (setq pycodechecker (concat user-emacs-directory "/flymake/pycheckers.py"))
(setq pycodechecker "pyflakes")
(after "flymake"
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
  (setq py-autopep8-options '("--ignore=W690" "--max-line-length=79"))
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

;;**
;;** formatting python code with yapf
;; (require 'py-yapf)              ; use yapf to beautify a Python buffer
;; discarding py-yapf in favor of yapfiy as py-yapf loses the kill ring when it runs
;;
(require-package 'yapfify)
;; (add-hook 'python-mode-hook 'yapf-mode)

;;;;;;;;;;;;;;;;;;;;;;; python virtual environment setup ;;;;;;;;;;;;;;;;;;;;;;;
(require 'virtualenvwrapper)           ;; python virtualenv wrapper
(venv-initialize-interactive-shells)   ;; if you want interactive shell support
(venv-initialize-eshell)               ;; if you want eshell support
;;(setq python-shell-virtualenv-path "~/.virtualenvs/default")

;;** flycheck for virtualenv
(after "flycheck"
    (defun flycheck-virtualenv-setup ()
      "Setup Flycheck for the current virtualenv."
      (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup)
      (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find)))

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

;;**
;;**  grand unified debugger
;;**  Tell Python debugger (pdb) to use the current virtual environment
;;**  https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
;;(setq gud-pdb-command-name "python3 -m pdb ")


;;**
;;** prettify symbols
;;**  when hovered show original
(setq prettify-symbols-unprettify-at-point 'right-edge)
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


;;**
;;** for imenu integration
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


;;**
;;** pydoc with helm interface
(use-package helm-pydoc
  :after helm)


;;**
;;** python import sort
(require 'py-isort)
(defun python-mode-sort-before-save-hook ()
  "Sort the python imports on save."
  (when (eq major-mode 'python-mode)
    (py-isort-before-save)))
(add-hook 'before-save-hook 'python-mode-sort-before-save-hook)
(setq py-isort-options '("--lines=80"))

(require 'pyimpsort)
(eval-after-load 'python
  '(define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer))

;;**
;;** set RET to newline and indent
(define-key python-mode-map (kbd "RET") 'newline-and-indent)

;;**
;;** add support for smartparens
(after 'smartparens
  (require 'smartparens-python))

(use-package python-docstring
  :config
  (python-docstring-install)
  :hook
  ((python-mode . python-docstring-mode)))

;;**
;; documentation and help -- enables eldoc for all
(add-hook 'python-mode-hook #'eldoc-mode)


(provide 'python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; python-config.el ends here
