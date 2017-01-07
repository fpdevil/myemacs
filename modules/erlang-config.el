;;; package --- erlang configuration settings
;;;
;;; Commentary:
;;;
;;; Filename: erlang-config.el
;;; Description: A major mode erlang language support in Emacs
;;;
;;; elisp code for erlang language support and handling
;;===========================================================================
;;; first load the standard and erlang specific libraries
(require 'cl)
(require 'cl-lib)
(require 'imenu)
(require 'ivy-erlang-complete)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start defining the emacs bindings for erlang                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.erl?$"        . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$"        . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.app\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.es\\'"      . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.xrl\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.yrl\\'"     . erlang-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; erlang compilation options                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add include directory to default compile path.
(defvar erlang-compile-extra-opts
  '(bin_opt_info debug_info (i . "../include")
                            (i . "../deps")
                            (i . "../../")
                            (i . "../../../deps")))

; define where put beam files.
(setq erlang-compile-outdir "../ebin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; context sensitive completion for erlang without connecting to erlang nodes    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
;; automatic update completion data after save
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
;;
(setq flycheck-erlang-include-path '("../include" "../deps"))

(defun fix-erlang-project-includes (project-root)
  "Find erlang include paths for PROJECT-ROOT with project deps."
  (setq-local flycheck-erlang-include-path
              (append
               (s-split
                "\n"
                (shell-command-to-string
                 (concat "find "
                         project-root
                         "/*"
                         " -type d -name include"))
                t)
               (list project-root
                     (concat project-root "/include")
                     (concat project-root "/deps")
                     default-directory
                     (concat
                      (locate-dominating-file
                       default-directory
                       "src") "include")
                     (concat
                      (locate-dominating-file
                       default-directory
                       "src") "deps")))))

(defun fix-erlang-project-code-path (project-root)
  "Find erlang include paths for PROJECT-ROOT with project deps."
  (let ((code-path
           (split-string (shell-command-to-string
                        (concat "find " project-root " -type d -name ebin")))
         ))
    (setq-local flycheck-erlang-library-path code-path)))

(require 'ivy-erlang-complete)
(defun my-erlang-hook ()
  "Setup for erlang."
  (let ((project-root (ivy-erlang-complete-autosetup-project-root)))
      (fix-erlang-project-code-path project-root)
      (fix-erlang-project-includes project-root))
  (ivy-erlang-complete-init))
(add-hook 'erlang-mode-hook #'my-erlang-hook)
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; erlang binaries path setup                                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "/usr/local/opt/erlang/lib/erlang/lib/tools-*/emacs"
load-path))
(setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang")
;; below conditional code is needed for proper elang load
(if
    (not (boundp 'erlang-root-dir))
    (message "Skipping erlang-mode: erlang-root-dir not defined")
  (progn
    (set 'erlang-bin (concat erlang-root-dir "/bin/"))
    (set 'erlang-lib (concat erlang-root-dir "/lib/"))
    (if
        (not (boundp 'erlang-mode-path))
        (set 'erlang-mode-path
             (concat
              erlang-lib
              (file-name-completion "tools-" erlang-lib)
              "emacs/erlang.el")))
    (if
        (and
         (file-readable-p erlang-mode-path)
         (file-readable-p erlang-bin))
        (progn
          (message "Setting up erlang-mode")
          (set 'exec-path (cons erlang-bin exec-path))
          (set 'load-path (cons
                           (concat
                            erlang-lib
                            (file-name-completion "tools-" erlang-lib)
                            "emacs")
                           load-path))
          (set 'load-path (cons (file-name-directory erlang-mode-path) load-path))
          (require 'erlang-start))
      (message "Skipping erlang-mode: %s and/or %s not readable"
erlang-bin erlang-mode-path))))

(setq erlang-man-root-dir "/usr/local/opt/erlang/lib/erlang/man")
(setq exec-path (cons "/usr/local/opt/erlang/lib/erlang/bin" exec-path))
(require 'erlang-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distel setup for erlang code auto-completion                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((distel-dir "/opt/erlang/distel/elisp"))
    (unless (member distel-dir load-path)
      ;; add distel-dir to the end of load-path
      (setq load-path (append load-path (list distel-dir)))))

; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)

; {{{
; distel-node
; distel node connection launch with (^C-^D-n)
(when (locate-library "distel")
  (require 'distel)
  (distel-setup)
  ;; (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
  (add-hook 'erlang-mode-hook
            '(lambda ()
               (unless erl-nodename-cache
                 (distel-load-shell))))

  (defun distel-load-shell ()
    "Load/reload the erlang shell connection to a distel node"
    (interactive)
    ;; Set default distel node name
    (setq erl-nodename-cache 'distel@localhost)
    (setq distel-modeline-node "distel")
    (force-mode-line-update)
    ;; Start up an inferior erlang with node name `distel'
    (let ((file-buffer (current-buffer))
          (file-window (selected-window)))
      (setq inferior-erlang-machine-options '("-name" "distel@localhost"))
      (switch-to-buffer-other-window file-buffer)
      (inferior-erlang)
      (select-window file-window)
      (switch-to-buffer file-buffer))))
; }}}

;; for imenu
(defun imenu-erlang-mode-hook()
    (imenu-add-to-menubar "imenu"))
(add-hook 'erlang-mode-hook 'imenu-erlang-mode-hook)

(add-hook 'erlang-shell-mode-hook
  (lambda ()
    ;; add some Distel bindings to the Erlang shell
    (dolist (spec distel-shell-keys)
    (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erlang ide set-up and erlang auto-completion using auto-complete and distel   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'auto-complete)
;(require 'auto-complete-config)
(require 'auto-complete-distel)
(ac-config-default)
(add-to-list 'ac-sources 'auto-complete-distel)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")

(setq ac-auto-show-menu    0.2)
(setq ac-delay             0.2)
(setq ac-menu-height       20)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)
(setq ac-show-menu-immediately-on-auto-complete t)

; Erlang auto-complete
(add-to-list 'ac-modes 'erlang-mode)

;;
; auto-complete-mode so can interact with inferior erlang and
; popup completion turn on when needed.
;;
(add-hook 'erlang-mode-hook (lambda () (auto-complete-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erlang auto completion using company mode and distel                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(require 'company-distel)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-distel))

; render company's doc-buffer (default <F1> when on a completion-candidate)
; in a small popup (using popup.el) instead of showing the whole help-buffer.
(setq company-distel-popup-help t)
; specify the height of the help popup created by company
(setq company-distel-popup-height 30)
; get documentation from internet
(setq distel-completion-get-doc-from-internet t)
; Change completion symbols
(setq distel-completion-valid-syntax "a-zA-Z:_-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A number of the erlang-extended-mode key bindings are useful in the shell too ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on the fly source code checking through flymake                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(require 'erlang-flymake)
(setq flymake-log-level 3)
(setq erlang-flymake-location (concat emacs-dir "/flymake/eflymake"))

(defun flymake-syntaxerl ()
 (flymake-compile-script-path "/opt/erlang/syntaxerl"))

(defun flymake-erlang-init ()
  "Erlang flymake compilation settings."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
       'flymake-create-temp-inplace))
  (local-file (file-relative-name temp-file
  (file-name-directory buffer-file-name)))
     (escript-exe (concat erlang-root-dir "/bin/escript"))
  (eflymake-loc (expand-file-name erlang-flymake-location)))
  (if (not (file-exists-p eflymake-loc))
        (error "Please set erlang-flymake-location to an actual location")
  (list escript-exe(list eflymake-loc local-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable flymake only for erlang mode                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
(defun flymake-erlang-mode-hook ()
  "Set erlang flymake mode."
  (flymake-mode 1))
(add-hook 'erlang-mode-hook 'flymake-erlang-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/ten0s/syntaxerl                                            ;;
;; see /usr/local/lib/erlang/lib/tools-<Ver>/emacs/erlang-flymake.erl            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun erlang-flymake-only-on-save ()
  "Trigger flymake only when the buffer is saved - clears syntax checker on a newline and when there is no change."
  (interactive)
  (setq flymake-no-changes-timeout most-positive-fixnum)
  (setq flymake-start-syntax-check-on-newline nil))

(erlang-flymake-only-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit settings                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'erlang
  '(progn
     (setq erlang-indent-level 4)
     (define-key erlang-mode-map "{" 'paredit-open-curly)
     (define-key erlang-mode-map "}" 'paredit-close-curly)
     (define-key erlang-mode-map "[" 'paredit-open-bracket)
     (define-key erlang-mode-map "]" 'paredit-close-bracket)
     (define-key erlang-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key erlang-mode-map (kbd "RET")
       'reindent-then-newline-and-indent)))



(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)

(provide 'erlang-config)
;;; erlang-config.el ends here
