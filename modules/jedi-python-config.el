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
;;;=============================================================================


;; global JEDI config vars
(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")

(defvar jedi-config:use-python3 t
  "Will use python3 for Jedi server.")

(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")

;; Helper functions

;; Small helper to scrape text from shell output
(defun get-shell-output (cmd)
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

;; Ensure that PATH is taken from shell
;; Necessary on some environments without virtualenv
;; Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  ;; (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

;;; ensure bashrc is loaded in emacs
(when window-system (set-exec-path-from-shell-PATH))

;; Package specific initialization
(add-hook
 'after-init-hook
 '(lambda ()
    ;; Looks like you need Emacs 24 for projectile
    (unless (< emacs-major-version 24)
      (require 'projectile)
      (projectile-global-mode))

    ;; Auto-complete
    (require 'auto-complete-config)
    (ac-config-default)

    ;; Uncomment next line if you like the menu right away
    (setq ac-show-menu-immediately-on-auto-complete t)

    ;; Can also express in terms of ac-delay var, e.g.:
    ;;   (setq ac-auto-show-menu (* ac-delay 2))

    ;; company backend(s) setup for python jedi
    (require 'company-jedi)
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

    ;; JEDI
    (require 'jedi)

    ;; (Many) config helpers follow

    ;; Alternative methods of finding the current project root
    ;; Method 1: basic
    (defun get-project-root (buf repo-file &optional init-file)
      "Just uses the vc-find-root function to figure out the project root.
       Won't always work for some directory layouts."
      (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
       (project-root (vc-find-root buf-dir repo-file)))
  (if project-root
      (expand-file-name project-root)
    nil)))

    ;; Method 2: slightly more robust
    (defun get-project-root-with-file (buf repo-file &optional init-file)
      "Guesses that the python root is the less 'deep' of either:
         -- the root directory of the repository, or
         -- the directory before the first directory after the root
            having the init-file file (e.g., '__init__.py'."

      ;; make list of directories from root, removing empty
      (defun make-dir-list (path)
        (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
                          (split-string path "/"))))
      ;; convert a list of directories to a path starting at "/"
      (defun dir-list-to-path (dirs)
        (mapconcat 'identity (cons "" dirs) "/"))
      ;; a little something to try to find the "best" root directory
      (defun try-find-best-root (base-dir buffer-dir current)
        (cond
         (base-dir ;; traverse until we reach the base
          (try-find-best-root (cdr base-dir) (cdr buffer-dir)
                              (append current (list (car buffer-dir)))))

         (buffer-dir ;; try until we hit the current directory
          (let* ((next-dir (append current (list (car buffer-dir))))
                 (file-file (concat (dir-list-to-path next-dir) "/" init-file)))
            (if (file-exists-p file-file)
                (dir-list-to-path current)
              (try-find-best-root nil (cdr buffer-dir) next-dir))))

         (t nil)))

      (let* ((buffer-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
             (vc-root-dir (vc-find-root buffer-dir repo-file)))
        (if (and init-file vc-root-dir)
            (try-find-best-root
             (make-dir-list (expand-file-name vc-root-dir))
             (make-dir-list buffer-dir)
             '())
          vc-root-dir))) ;; default to vc root if init file not given

    ;; Set this variable to find project root
    (defvar jedi-config:find-root-function 'get-project-root-with-file)

    (defun current-buffer-project-root ()
      (funcall jedi-config:find-root-function
               (current-buffer)
               jedi-config:vcs-root-sentinel
               jedi-config:python-module-sentinel))

    (defun jedi-config:setup-server-args ()
      ;; little helper macro for building the arglist
      (defmacro add-args (arg-list arg-name arg-value)
        `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
      ;; and now define the args
      (let ((project-root (current-buffer-project-root)))

        (make-local-variable 'jedi:server-args)

        (when project-root
          (message (format "Adding system path: %s" project-root))
          (add-args jedi:server-args "--sys-path" project-root))

        (when jedi-config:with-virtualenv
          (message (format "Adding virtualenv: %s" jedi-config:with-virtualenv))
          (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))

    ;; Use system python
    (defun jedi-config:set-python-executable ()
      (set-exec-path-from-shell-PATH)
      (make-local-variable 'jedi:server-command)
      (set 'jedi:server-command
           (list (executable-find "python") ;; may need help if running from GUI
                 (cadr default-jedi-server-command))))

    ;; use python3 based jedi
    (defun jedi-config:set-python3-executable ()
      (set-exec-path-from-shell-PATH)
      (set py-python-command (executable-find "python3"))
      (make-local-variable 'jedi:server-command)
      (make-local-variable 'jedi:server-script)
      (set 'jedi:server-script (concat python-environment-directory "/jedi/bin/jediepcserver.py"))
      (set 'jedi:server-command
           (list (executable-find "python3") jedi:server-script)))

    ;; Now hook everything up
    ;; Hook up to auto complete
    (add-to-list 'ac-sources 'ac-source-jedi-direct)

    ;; Enable Jedi setup on mode start
    (add-hook 'python-mode-hook 'jedi:setup)

    ;; Buffer-specific server options
    (add-hook 'python-mode-hook
              'jedi-config:setup-server-args)

    (when jedi-config:use-system-python
      (message (format "loading Jedi for system python 2.x.x %s" jedi-config:use-system-python))
      (add-hook 'python-mode-hook
                'jedi-config:set-python-executable))

    ;; (when jedi-config:use-python3
    ;;  (message (format "Loading Jedi for python 3.x.x %s" jedi-config:use-python3))
    ;;  (add-hook 'python-mode-hook
    ;;            'jedi-config:set-python3-executable))

    ;; And custom keybindings
    (defun jedi-config:setup-keys ()
      "Custom keyboard mapping for jedi."
      (local-set-key (kbd "M-.") 'jedi:goto-definition)
      (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
      (local-set-key (kbd "M-?") 'jedi:show-doc)
      (local-set-key (kbd "M-/") 'jedi:get-in-function-call))

    ;; Don't let tooltip show up automatically
    (setq jedi:get-in-function-call-delay 10000000)
    ;; Start completion at method dot
    (setq jedi:complete-on-dot t)
    ;; for call signature display
    (setq jedi:tooltip-method '(pos-tip popup)
          jedi:tooltip-show '(pos-tip popup))
    ;; create imenu index
    (setq jedi:install-imenu t)
    ;; major mode for showing documentation
    (setq jedi:doc-mode 'rst-mode)
    ;; python environment
    (setq jedi:environment-root "jedi")

    ;; virtual environment set-up
    (setq jedi:environment-virtualenv
          (append python-environment-virtualenv
                  '("--python" "/usr/local/bin/python3")))

    ;; jedi logging with debug
    (setq jedi:server-args '("--log-level" "DEBUG"
                             "--log-traceback"))
    ;; Use custom key-binds
    (add-hook 'python-mode-hook 'jedi-config:setup-keys)))

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
(provide 'jedi-python-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; jedi-python-config.el ends here
