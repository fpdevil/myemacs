;;; package --- customize python configuration for Emacs using JEDI
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;            Python 3 support
;;;
;;; Filename   : jedi-python-config.el
;;; Description: Python configuration for Emacs using Jedi
;;;              A full featured python ide and language support for Aquamacs
;;; https://github.com/wernerandrew/jedi-starter/blob/master/jedi-starter.el
;;; http://wikemacs.org/wiki/Python
;;;
;;; Code:
;;;

;; include the necessary libraries
(require 'jedi)                       ; a Python auto-completion for Emacs
(require 'epc)                        ; RPC stack for the Emacs Lisp


;; ** small helper to scrape the text from shell output
(defun get-shell-output (cmd)
  "CMD take a shell command and stringify."
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

;; ** [PATH] - Ensure that PATH is taken from the shell, which might be
;;    required on some environments without virtualenv.
;; ref: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun jedi-config:set-python-executable ()
  "Setting up the PATH."
  (set-exec-path-from-shell-PATH)
  (make-local-variable 'jedi:server-command)
  ;; command used to run Jedi server
  (setq jedi:server-command (list (executable-find "python3") jedi:server-script)))

;; ** auto-complete integration configuration for jedi
(after "auto-complete"
  ;;(require 'auto-complete-config)
  ;;(ac-config-default)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (setq ac-auto-show-menu (* ac-delay 2))
  (setq ac-quick-help-delay 0.2)
  (setq ac-sources '(ac-source-jedi-direct
                     ac-source-features
                     ac-source-abbrev
                     ac-source-filename
                     ))
  (setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources)))

;; ** company integration configuration for jedi
(after "company"
  (require 'company-jedi)
  (defun config/company-jedi-setup()
    "Configure company-backends for company-jedi."
    (add-to-list 'company-backends 'company-jedi)
    (setq-local company-backends (delete 'company-bbdb company-backends))
    ;;(setq-local company-minimum-prefix-length 0)
    (setq-local company-quickhelp-delay 0.5)
    (setq-local company-minimum-prefix-length 2)
    (setq-local company-selection-wrap-around t))

  ;; now add the backends to python-mode
  (add-hook 'python-mode-hook 'config/company-jedi-setup))


;; ** jedi code auto completion
(add-hook 'python-mode-hook 'jedi:setup)         ;;-- for company
;;
;; switched over to auto-complete as jedi was not yet ready for 3.7 python
;; and company was not showing completions. also added support for 3.7 manually
;; in the jedi module at /usr/local/lib/python3.7/site-packages/jedi/api/environment.py
(add-hook 'python-mode-hook 'jedi:ac-setup)      ;;-- for auto-complete

;; ** start completion at method dot
(setq jedi:complete-on-dot t)

;; ** do not let the tooltip show up automatically
;; (setq jedi:get-in-function-call-delay 10000000)
(setq jedi:get-in-function-call-delay 0.2)

;; ** which python environment to use
(setq jedi:environment-root "jedi")

;; ** jedi logging with debug
(setq jedi:server-args '("--log-level" "DEBUG" "--log-traceback"))

;; ** [imenu] - currently works only with DEV branch
(setq jedi:install-imenu t)
(setq jedi:imenu-create-index-function 'jedi:create-nested-imenu-index)
;;(setq jedi:imenu-create-index-function 'jedi:create-flat-imenu-index)

;; ** installation of the dev branch jedi
;; (jedi:install-python-jedi-dev-command "pip"
;;                                       "install"
;;                                       "--upgrade"
;;                                       "git+https://github.com/davidhalter/jedi.git@dev#egg=jedi")

;; ** virtual environment setup
(setq jedi:environment-virtualenv
     (append python-environment-virtualenv '("--python" "/usr/local/bin/python3")))

;; ** for call signature display
(setq jedi:tooltip-method nil                   ; get eldoc style signature hints
      jedi:tooltip-method '(pos-tip popup)    ; get popup style signature hints
      jedi:tooltip-show '(pos-tip popup))


;; ** setup the jedi additional configuration options as needed
(add-hook 'python-mode-hook
          '(lambda()
             ;; set appropriate python executable
             (jedi-config:set-python-executable)
             ;; custom kbd keys
             (jedi-config:setup-keys)
             ;; customize function argument face
             (set-face-attribute 'jedi:highlight-function-argument nil
                                 :foreground "green")
             ))

;; ** install the jedi private python environment at the specified location
(when
  (and
    (not (file-directory-p
           (locate-user-emacs-file ".python-environments")))
    (executable-find "virtualenv"))
  (jedi:install-server))

;; ** function for adding custom keybindings
(defun jedi-config:setup-keys ()
  "Custom keyboard mapping for jedi."
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-/") 'jedi:show-doc)
  (local-set-key (kbd "M-?") 'jedi:complete)
  (define-key python-mode-map "\C-cr" 'helm-jedi-related-names))


;; ** [jedi-dyrex] - Tree style source code viewer for Python buffer
(require-package 'jedi-direx)
(after "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'jedi-python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; jedi-python-config.el ends here
