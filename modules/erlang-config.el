;;===============================================================
;;; main configuration file for erlang mode
;; Filename: erlang-config.el
;; Description: A major mode erlang language support in Emacs
;;
;;; Commentary:
;;
;; elisp code for erlang language support and handling
;;===============================================================


;;
; erlang settings for Emacs
;;
;;------------------------------------------------------------------------
;;
; standard libraries
;;
(require 'cl)
(require 'cl-lib)
(require 'imenu)
;;------------------------------------------------------------------------


;;; Code:
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))


;;
; erlang path setup
;;
(setq load-path (cons "/opt/erlang/r19.0/lib/tools-*/emacs"
load-path))
(setq erlang-root-dir "/opt/erlang/r19.0")
(setq erlang-man-root-dir "/opt/erlang/r19.0/man")
(setq exec-path (cons "/opt/erlang/r19.0/bin" exec-path))
(require 'erlang-start)



;;
; distel setup
;;
; (add-to-list 'load-path "/opt/erlang/r19.0/distel/elisp")
; (require 'distel)
; (distel-setup)

(let ((distel-dir "/opt/erlang/r19.0/distel/elisp"))
    (unless (member distel-dir load-path)
;; Add distel-dir to the end of load-path
(setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)
;;------------------------------------------------------------------------

;; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)


;; when starting an Erlang shell in Emacs, default in the node name
;; default node name to emacs@localhost
(setq inferior-erlang-machine-options '("-sname" "emacs"))

;; add Erlang functions to an imenu menu
(defun imenu-erlang-mode-hook()
    (imenu-add-to-menubar "imenu"))
(add-hook 'erlang-mode-hook 'imenu-erlang-mode-hook)


;;------------------------------------------------------------------------
;; ref http://bob.ippoli.to/archives/2007/03/16/distel-and-erlang-mode-for-emacs-on-mac-os-x/
;; tell distel to default to that node
;;------------------------------------------------------------------------
; (setq erl-nodename-cache
;       (make-symbol
;        (concat
;         "emacs@"
;         ;; Mac OS X uses "name.local" instead of "name", this should work
;         ;; pretty much anywhere without having to muck with NetInfo
;         ;; ... but I only tested it on Mac OS X.
;         (car (split-string (shell-command-to-string "hostname"))))))


;; short host name, like `hostname -s`, remote shell likes this better
(defun short-host-name ()
  (string-match "[^\.]+" system-name)
  (substring system-name (match-beginning 0) (match-end 0)))

;; set default nodename for distel (= also for erlang-shell-remote)
(setq erl-nodename-cache (intern (concat "emacs" "@" (short-host-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; erlang ide set-up and
; erlang auto-completion using auto-complete and distel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; erlang auto completion using company mode and distel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (require 'company)
; (add-hook 'after-init-hook 'global-company-mode)
; (require 'company-distel)
; (add-to-list 'company-backends 'company-distel)

; ; render company's doc-buffer (default <F1> when on a completion-candidate)
; ; in a small popup (using popup.el) instead of showing the whole help-buffer.
; (setq company-distel-popup-help t)
; ; specify the height of the help popup created by company
; (setq company-distel-popup-height 30)
; ; get documentation from internet
; (setq distel-completion-get-doc-from-internet t)
; ; Change completion symbols
; (setq distel-completion-valid-syntax "a-zA-Z:_-")

;;------------------------------------------------------------------------
;;
; A number of the erlang-extended-mode key bindings are useful in the shell too
;;
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")


; (add-hook 'erlang-shell-mode-hook
;           (lambda ()
;             ;; add some Distel bindings to the Erlang shell
;             (dolist (spec distel-shell-keys)
;               (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; on the fly source code checking through flymake
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(require 'erlang-flymake)
(setq flymake-log-level 3)
(setq erlang-flymake-location (concat emacs-dir "/flymake/eflymake"))
;(setq erlang-flymake-location "~/.emacs.d/flymake/eflymake")

(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
       'flymake-create-temp-inplace))
  (local-file (file-relative-name temp-file
  (file-name-directory buffer-file-name)))
     (escript-exe (concat erlang-root-dir "/bin/escript"))
  (eflymake-loc (expand-file-name erlang-flymake-location)))
  (if (not (file-exists-p eflymake-loc))
        (error "Please set erlang-flymake-location to an actual location")
  (list escript-exe(list eflymake-loc local-file)))))
;;
; enable flymake globally
;;
; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; enabling only erlang-mode
(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
(defun flymake-erlang-mode-hook ()
        (flymake-mode 1))
(add-hook 'erlang-mode-hook 'flymake-erlang-mode-hook)


(provide 'erlang-config)
;;; erlang-config.el ends here
