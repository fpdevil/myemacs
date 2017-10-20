;;; package --- customize core python 3.x.x configuration for Emacs
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : python-config.el
;;; Description: Core Python 3.x.x configuration for Emacs.
;;;              A full featured python ide and language support for Aquamacs
;;;=============================================================================

;; load all the syntax specific packages needed for python3
(require 'jedi)                 ; a Python auto-completion for Emacs
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
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rebind Enter to Ctrl+J for proper indentation                              ;;
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
      py-python-command (executable-find "python3"))
;; below line specified for handling the args-out-of-range error in buffer
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-check-command (executable-find "pyflakes"))
(setq python-environment-directory (concat (getenv "HOME") "/.emacs.d/.python-environments"))
;; handling white-spaces in python mode
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system path in the lisp                                                   ;;
;; set PATH, because we don't load .bashrc                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv
 "PATH" (concat
   "$HOME/bin:"
   "/bin:"
   "/usr/bin:"
   "/sbin:"
   "/usr/sbin:"
   "/usr/local/bin:"
   "/usr/local/sbin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set PYTHONPATH, because we don't load from .bashrc                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-pypath-from-shell-pythonpath ()
  "Set the PYTHONPATH variable as its not pulled from .profile."
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))

(if (not (getenv "PYTHONPATH")) (setenv "PYTHONPATH" (executable-find "python3")))

(if window-system (set-pypath-from-shell-pythonpath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python virtual environment setup                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)             ;; if you want eshell support
(if (equal (getenv "WORKON_HOME") nil)
    (setenv "WORKON_HOME" (concat (getenv "HOME") "/.virtualenvs")))
(setq pyvenv-activate (getenv "WORKON_HOME"))
(setq venv-location
      (expand-file-name (concat (getenv "HOME") "/.virtualenvs/")))
(setq python-shell-virtualenv-root (concat (getenv "HOME") "/.virtualenvs/"))
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
    (message "running flymake-pylint-init")
    (let ((local-directory (file-name-directory buffer-file-name)))
      (if (file-writable-p local-directory)
          (let* ((temp-file (flymake-init-create-temp-buffer-copy
                             'flymake-create-temp-inplace))
                 (local-file (file-relative-name
                              temp-file
                              (file-name-directory buffer-file-name))))
            ;; (list "pep8" (list "--repeat" local-file))
            (list (executable-find "epylint") (list local-file)))
        (list "cat" (list "/dev/null")))))

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
;; python documentation under symbols pydoc-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pydoc-info)
(info-lookup-add-help
   :mode 'python-mode
   :parse-rule 'pydoc-info-python-symbol-at-point
   :doc-spec
   '(("(python)Index" pydoc-info-lookup-transform-entry)
     ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load python helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat module-dir "/python-helper-config.el"))
(require 'python-helper-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; python-config.el ends here
