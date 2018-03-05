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
;;;=============================================================================
(require 'jedi)                 ; a Python auto-completion for Emacs

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook
          '(lambda()
             (jedi:setup)
             (setq jedi:complete-on-dot t)
             (setq jedi:environment-root "jedi")
             ;; jedi logging with debug
             (setq jedi:server-args
                   '("--log-level" "DEBUG"
                     "--log-traceback"))
             ;; imenu - currently works only with DEV branch
             (setq jedi:install-imenu t)
             ;; auto-completion sources
             (after 'auto-complete
               (setq ac-sources
                     (delete 'ac-source-words-in-same-mode-buffers ac-sources))
               (add-to-list 'ac-sources 'ac-source-filename)
               (add-to-list 'ac-sources 'ac-source-jedi-direct))
             ;; virtual environment setup
             (setq jedi:environment-virtualenv
                   (append python-environment-virtualenv
                           '("--python" "/usr/local/bin/python3")))
             ;; custom kbd keys
             (jedi-config:setup-keys)
             ;; for call signature display
             (setq jedi:tooltip-method '(pos-tip popup)
                   jedi:tooltip-show '(pos-tip popup))))

;; install the jedi python environment
(when
  (and
    (not (file-directory-p
           (locate-user-emacs-file ".python-environments")))
    (executable-find "virtualenv"))
  (jedi:install-server))

;; function to add custom keybindings
(defun jedi-config:setup-keys ()
  "Custom keyboard mapping for jedi."
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (local-set-key (kbd "M-/") 'jedi:get-in-function-call))

;; -- auto-complete integration for jedi
(after 'auto-complete
    (require 'auto-complete-config)
    (ac-config-default))

;; -- company integration for jedi
(after 'company
    (require 'company-jedi)
    (defun my-python-hooks()
        '(progn
            (unless (member 'company-jedi (car company-backends))
                (setq comp-back (car company-backends))
                (push 'company-jedi comp-back)
                (setq company-backends (list comp-back))
                (setq company-backends (delete 'company-bbdb company-backends)))))
    (add-hook 'python-mode-hook 'my-python-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'jedi-python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; jedi-python-config.el ends here
