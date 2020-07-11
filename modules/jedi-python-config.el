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

;; load jedi and epc (rpc stack for Emacs lisp)
(require 'jedi)
(require 'epc)


;; ** small helper to scrape the text from shell output
(defun get-shell-output (cmd)
  "CMD take a shell command and stringify."
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

;; ** [PATH] - Ensure that PATH is taken from the shell, which might be
;;    required on some environments without virtualenv.
;;    ref: http://stackoverflow.com/questions/8606954
;;      /path-and-exec-path-set-but-emacs-does-not-find-executable
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

;; ** [auto-complete-mode] jedi auto-complete integration
;;(when (eq dotemacs-completion-engine 'auto-complete)
(after "auto-complete"
  (require 'auto-complete-config)
  (ac-config-default)
  (add-hook 'python-mode-hook 'auto-complete-mode)
  (add-hook 'inferior-python-mode-hook 'auto-complete-mode)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (setq ac-auto-show-menu (* ac-delay 2))
  (setq ac-quick-help-delay 0.2)
  (setq ac-sources
        (append
         '(ac-source-jedi-direct
           ac-source-features
           ac-source-abbrev
           ac-source-filename)
         ac-sources))
  ;; auto completion in python repl
  (add-to-list 'ac-modes 'inferior-python-mode)
  (add-hook 'inferior-python-mode-hook 'ac-capf-setup)
  ;;(setq ac-sources (delete 'ac-source-words-in-same-mode-buffers ac-sources))
  (add-hook 'python-mode-hook
            (lambda ()
              (delq 'ac-source-dictionary ac-sources)
              (delq 'ac-source-abbrev ac-sources)
              (delq 'ac-source-words-in-same-mode-buffers ac-sources)))
  ;; switched over to auto-complete as jedi was not yet ready for 3.7 python
  ;; and company was not showing completions. also added support for 3.7 manually
  ;; in the jedi module at /usr/local/lib/python3.7/site-packages/jedi/api/environment.py
  (add-hook 'python-mode-hook 'jedi:ac-setup))


;; ** [company-mode] jedi company-mode integration
;;(when (eq dotemacs-completion-engine 'company)
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
  (add-hook 'python-mode-hook 'config/company-jedi-setup)

  ;; disable/enable company in python repl
  (add-hook 'inferior-python-mode-hook (lambda () (company-mode -1)))

  ;; company jedi start
  (add-hook 'python-mode-hook 'jedi:setup))


;; ** jedi code auto completion
;;(add-hook 'python-mode-hook 'jedi:setup)         ;;-- for company
;;(add-hook 'python-mode-hook 'jedi:ac-setup)      ;;-- for auto-complete


;; ** start completion at method dot
(setq jedi:complete-on-dot t)

;; ** do not let the tooltip show up automatically
;; (setq jedi:get-in-function-call-delay 10000000)
(setq jedi:get-in-function-call-delay 0.2)

;; ** which python environment to use
(setq jedi:environment-root "jedi")

;; ** jedi logging with debug
(setq jedi:server-args '("--log-level" "ERROR" "--log-traceback" "--log" "/tmp/jedi.log"))

;; ** [imenu] - currently works only with DEV branch
(setq jedi:install-imenu t)
(setq jedi:imenu-create-index-function 'jedi:create-nested-imenu-index)

;; ** virtualenv setup command
(setcar jedi:install-server--command "pip3")

;; ** virtual environment setup
(setq jedi:environment-virtualenv
      (append python-environment-virtualenv '("--python" "/usr/local/bin/python3")))

;; ** if installing the dev version, uncomment below
;; (jedi:install-python-jedi-dev)

;; ** for call signature display
(setq jedi:tooltip-method nil                 ; get eldoc style signature hints
      jedi:tooltip-method '(pos-tip popup)    ; get popup style signature hints
      jedi:tooltip-show '(pos-tip popup))


;; ** setup the jedi additional configuration options as needed
(add-hook 'python-mode-hook
          '(lambda()
             ;; set appropriate python executable
             (jedi-config:set-python-executable)
             ;; custom kbd keys
             (setq jedi:use-shortcuts t)
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

;;**
;;** jedi eldoc for documentation
(defgroup jedi-eldoc nil
  "Eldoc for jedi"
  :group 'jedi
  :prefix "jedi-eldoc:")

(defface jedi-eldoc:highlight-function-argument
  '((t (:inherit eldoc-highlight-function-argument)))
  "Face of current function argument"
  :group 'jedi-eldoc)

(defun* jedi-eldoc:format--for-eldoc (&key params index call_name)
  (let ((current-arg (nth index params)))
    (if current-arg
        (setf (nth index params)
              (propertize current-arg
                          'face 'jedi-eldoc:highlight-function-argument)))
    (concat call_name "(" (mapconcat #'identity params ", ") ")")))

(defun jedi-eldoc:format-for-eldoc (args)
  (when args
    (eldoc-message (apply #'jedi-eldoc:format--for-eldoc args))))

(defun jedi-eldoc:documentation-function ()
  (deferred:nextc
    (jedi:call-deferred 'get_in_function_call)
    #'jedi-eldoc:format-for-eldoc)
  nil)

(defun jedi-eldoc:init ()
  ;; disable jedi's popup documentation
  ;; (setq jedi:get-in-function-call--d t)
  (set (make-local-variable 'eldoc-documentation-function)
       #'jedi-eldoc:documentation-function)
  (turn-on-eldoc-mode))

;; now enable jedi eldoc
(jedi-eldoc:init)
(set-face-attribute 'jedi-eldoc:highlight-function-argument nil
                    :foreground "green")



(provide 'jedi-python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; jedi-python-config.el ends here
