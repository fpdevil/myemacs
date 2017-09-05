;;; package --- erlang configuration settings
;;;
;;; Commentary:
;;;
;;; Filename: erlang-config.el
;;; Description: A major mode erlang language support in Emacs
;;;
;;; elisp code for erlang language support and handling
;;===========================================================================

(require 'cl)
(require 'cl-lib)
(require 'imenu)
(require 'company-erlang)

;;;
;;; Code:
;;;

(defconst erlang-emacs-major-version 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle the xemacs warnings                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst erlang-xemacs-p (string-match "Lucid\\|XEmacs" emacs-version)
  "Non-nil when running under XEmacs or Lucid Emacs."
  )

(defvar erlang-xemacs-popup-menu '("Erlang Mode Commands" . nil)
  "Common popup menu for all buffers in Erlang mode. This variable is
  destructively modified every time the Erlang menu is modified. The
  effect is that all changes take effect in all buffers in Erlang mode,
  just like under GNU Emacs. Never EVER set this variable!"
  )


;;--------------------------erlang---------------
(setq load-path (cons "/usr/local/opt/erlang/lib/erlang/lib/tools-2.6.7/emacs" load-path))
(setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang")
(setq exec-path (cons "/usr/local/opt/erlang/lib/erlang/bin" exec-path))
(setq erlang-man-root-dir "/usr/local/opt/erlang/lib/erlang/man")
(setq erlang-compile-extra-opts (list 'debug_info))
(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

;; --------- wrangler -----------------------
(add-to-list 'load-path "/usr/local/lib/erlang/lib/wrangler-1.2.0/elisp")
(require 'wrangler)
(add-hook 'erlang-mode-hook 'erlang-wrangler-on)

;;----------------------distel-------------------

(let ((distel-dir "/usr/local/share/distel/elisp"))
    (unless (member distel-dir load-path)
        ;; Add distel-dir to the end of load-path
        (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(require 'auto-complete-distel)
(distel-setup)

;; Some Erlang customizations

(add-hook 'erlang-mode-hook
  (lambda ()
  ;; when starting an Erlang shell in Emacs, default in the node name
    (setq inferior-erlang-machine-options '("-sname" "emacs@apple"))
    ;; add Erlang functions to an imenu menu
    (imenu-add-to-menubar "imenu")))

;; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)

;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
        (car (split-string (shell-command-to-string "hostname"))))))


;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
  (lambda ()
;; add some Distel bindings to the Erlang shell
        (dolist (spec distel-shell-keys)
        (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;------------------------esense--------------------

(setq load-path (cons "/opt/erlang/esense-1.12" load-path))
(require 'esense-start)
(setq esense-indexer-program "/opt/erlang/esense-1.12/esense.sh")

;;-------------------flymake-------------------

(setq flymake-log-level 3)
(setq erlang-flymake-location (concat emacs-dir "/flymake/eflymake"))


(defun flymake-erlang-init ()
  "Erlang flymake compilation settings."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                                         (file-name-directory buffer-file-name)))
         (list (concat (getenv "HOME") "/.emacs.d/flymake/emakefly"))
         (escript-exe (concat erlang-root-dir "/bin/escript"))
         (eflymake-loc (expand-file-name erlang-flymake-location)))
    (if (not (file-exists-p eflymake-loc))
        (error "Please set erlang-flymake-location to an actual location")
      (list escript-exe (list eflymake-loc local-file)))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))


(defun ebm-find-rebar-top-recr (dirname)
      (let* ((project-dir (locate-dominating-file dirname "rebar.config")))
        (if project-dir
            (let* ((parent-dir (file-name-directory (directory-file-name project-dir)))
                   (top-project-dir (if (and parent-dir (not (string= parent-dir "/")))
                                       (ebm-find-rebar-top-recr parent-dir)
                                      nil)))
              (if top-project-dir
                  top-project-dir
                project-dir))
              project-dir)))

    (defun ebm-find-rebar-top ()
      (interactive)
      (let* ((dirname (file-name-directory (buffer-file-name)))
             (project-dir (ebm-find-rebar-top-recr dirname)))
        (if project-dir
            project-dir
          (erlang-flymake-get-app-dir))))

     (defun ebm-directory-dirs (dir name)
        "Find all directories in DIR."
        (unless (file-directory-p dir)
          (error "Not a directory `%s'" dir))
        (let ((dir (directory-file-name dir))
              (dirs '())
              (files (directory-files dir nil nil t)))
            (dolist (file files)
              (unless (member file '("." ".."))
                (let ((absolute-path (expand-file-name (concat dir "/" file))))
                  (when (file-directory-p absolute-path)
                    (if (string= file name)
                        (setq dirs (append (cons absolute-path
                                                 (ebm-directory-dirs absolute-path name))
                                           dirs))
                        (setq dirs (append
                                    (ebm-directory-dirs absolute-path name)
                                    dirs)))))))
              dirs))

    (defun ebm-get-deps-code-path-dirs ()
        (ebm-directory-dirs (ebm-find-rebar-top) "ebin"))

    (defun ebm-get-deps-include-dirs ()
       (ebm-directory-dirs (ebm-find-rebar-top) "include"))

    (fset 'erlang-flymake-get-code-path-dirs 'ebm-get-deps-code-path-dirs)
    (fset 'erlang-flymake-get-include-dirs-function 'ebm-get-deps-include-dirs)
(add-hook 'find-file-hook 'flymake-find-file-hook)


;;----------------------------------------------------------------------


(setq erlang-electric-commands
      ; Insert a comma character and possibly a new indented line.
      '(erlang-electric-comma
      ; Insert a semicolon character and possibly a prototype for the next line.
        erlang-electric-semicolon
      ; Insert a '>'-sign and possible a new indented line.
        erlang-electric-gt
        ))


;; -------------------------------------------------------------------
;;                      Erlang KEY BINDINGS
;; -------------------------------------------------------------------
(add-hook 'erlang-mode-hook
          '(lambda ()
             (define-key erlang-mode-map (kbd "C-.")
               'erl-find-source-under-point)
             ))
(add-hook 'erlang-mode-hook
          '(lambda ()
             (define-key erlang-mode-map (kbd "C-,") 'erl-find-source-unwind)
             ))

;; auto complete
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-distel)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-distel))
;;(require 'company-distel-modules)

(add-hook 'erlang-mode-hook
    (lambda ()
      (setq company-backends '(company-distel))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'erlang-config)
;;; erlang-config.el ends here