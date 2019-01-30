;; package --- PYTHON CONFIGURATION
;;; Commentary
;;; Code:

;; * python configuration
;; The package is "python" but the mode is "python-mode":
(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c c")
              ("C-c C-z" . python-shell))
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
                      (match-string 1 item)))))))))

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil))
  (add-hook 'python-mode-hook 'my-python-mode-hook))


(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4
                  python-indent-offset 4
                  indent-tabs-mode nil)))

(setq-default pdb-command-name "python3 -m pdb")
(setq python-shell-interpreter "ipython3"
      py-python-command "python3"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \: "
      python-shell-prompt-output-regexp "Out\: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))")

;; * JEDI
(use-package jedi
  :ensure t
  :config
  (set-exec-path-from-shell-PATH)
  (setq jedi:server-command (list "python3" jedi:server-script))
  ;; virtual environment setup
  (setq jedi:environment-virtualenv
        (append python-environment-virtualenv '("--python" "/usr/local/bin/python3")))
  (setq jedi:complete-on-dot t)
  ;; python environment to use
  (setq jedi:environment-root "jedi")
  (setq jedi:server-args '("--log-level" "DEBUG" "--log-traceback"))
  ;; imenu - currently works only with DEV branch
  (setq jedi:install-imenu t
        jedi:imenu-create-index-function 'jedi:create-nested-imenu-index)

  ;; install the jedi python environment
  (when
      (and
       (not (file-directory-p
             (locate-user-emacs-file ".python-environments")))
       (executable-find "virtualenv"))
    (jedi:install-server))

  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-jedi-direct)

  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))


;;** helper functions
;;*** small helper to scrape text from shell output
(defun get-shell-output (cmd)
  "Scrape text from the shell output of the CMD."
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


;; * company-mode completion back-end for Python JEDI
(use-package company-jedi
  :ensure t
  :config
  (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (setq jedi:get-in-function-call-delay 0.2)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi))

;; * ELPY
(use-package elpy
  :ensure t
  :after (company python)
  :init (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi"
        elpy-rpc--backend-python-command "/usr/local/bin/python3"
        elpy-rpc-python-command "/usr/local/bin/python3"
        elpy-rpc-pythonpath "/usr/local/lib/python3.7/site-packages")
  (when (executable-find "ipython3")
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "-i --simple-prompt"))

  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules))

(provide 'my-python-config)
