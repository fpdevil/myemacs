;; PYTHON CONFIGURATION
;; --------------------------------------

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
        python-shell-interpreter "/usr/local/bin/ipython3"
        ;; if extras are needed with ipython3
        python-shell-interpreter-args "-i"
        ;; (setq python-shell-interpreter-args "--pylab")
        python-shell-interpreter-args (if (aqua/is-mac) "--matplotlib=osx --colors=Linux --simple-prompt --pprint"
                                      (if (aqua/is-linux) "--gui=wx --matplotlib=wx --colors=Linux"))

        python-shell-prompt-regexp ">>> "
        ;; additional shell options added
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "

        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"

        python-shell-completion-native-enable nil
        python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython" "python"))
        py-python-command (executable-find "python3")
        python-skeleton-autoinsert t)

        (untabify (point-min) (point-max))
        (hs-minor-mode t)                     ; hs-minor-mode
        (auto-fill-mode 0)                    ; auto-fill-mode
        (whitespace-mode t)                   ; whitespace-mode
        ;;(hl-line-mode t)                      ; hl-line-mode
        (set (make-local-variable 'electric-indent-mode) nil))

(add-hook 'python-mode-hook 'my-python-mode-config)


;; --
;; visual clue on how the code is indented
(require-package 'highlight-indentation)
(require 'highlight-indentation)
(defun hl-py-mode ()
  "Highlight indentation for python."
  (lambda ()
    (highlight-indentation-mode)
    (highlight-indentation-current-column-mode)))

(add-hook 'python-mode-hook 'hl-py-mode)



(elpy-enable)

(setq elpy-modules '(
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-pyvenv
                     elpy-module-highlight-indentation  ;; might break older emacs
                     elpy-module-sane-defaults
                     elpy-module-yasnippet
                     ))

(setq elpy-rpc-backend "rope"
      elpy-rpc-python-command "/usr/local/bin/python3"
      elpy-rpc-python-path "/usr/local/lib/python3.7/site-packages")

;; use flycheck not flymake with elpy
;; flymake integration if flycheck is not present
(if (require 'flycheck nil t)
    (lambda ()
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (set elpy-syntax-check-command "/usr/local/bin/flake8")
      (add-hook 'elpy-mode-hook 'flycheck-mode))
  (add-hook 'elpy-mode-hook
            '(lambda ()
               (progn
                 (add-to-list 'flymake-err-line-patterns '("\\([^|]+\\)| \\([^:]+\\):\\([0-9]+\\)$" 2 3 nil 1))
                 (set (make-local-variable 'flymake-warning-predicate) "^.[^EF]")))))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(provide 'my-python-config)
