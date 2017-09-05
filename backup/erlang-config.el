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
(require 'company-erlang)
(require 'ivy-erlang-complete)

(load-file (concat module-dir "/aqua-erlhelper-config.el"))
(require 'aqua-erlhelper-config)

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
(add-to-list 'auto-mode-alist '(".[eh]rl'"        . erlang-mode))
(add-to-list 'auto-mode-alist '(".yaws?'"         . erlang-mode))
(add-to-list 'auto-mode-alist '(".escript?'"      . erlang-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle the xemacs warnings                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst erlang-xemacs-p (string-match "Lucid\\|XEmacs" emacs-version)
  "Non-nil when running under XEmacs or Lucid Emacs.")

(defvar erlang-xemacs-popup-menu '("Erlang Mode Commands" . nil)
  "Common popup menu for all buffers in Erlang mode. This variable is
  destructively modified every time the Erlang menu is modified. The
  effect is that all changes take effect in all buffers in Erlang mode,
  just like under GNU Emacs. Never EVER set this variable!")

;; to calm down the wrangler error
(defconst erlang-emacs-major-version 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; erlang root and binaries path setup                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;{{{ set the erlang software locations first
(setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang")
(setq load-path
      (cons "/usr/local/opt/erlang/lib/erlang/lib/tools-*/emacs"
            load-path))
(load "erlang_appwiz" t nil)
(message "--> loading erlang root %s" erlang-root-dir)

;; below conditional code is needed for loading proper erlang vm
(if
    (not (boundp 'erlang-root-dir))
    (message "discarding the erlang-mode: erlang-root-dir not defined")
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
          (message "--> setting up erlang-mode from %s" erlang-root-dir)
          (set 'exec-path (cons erlang-bin exec-path))
          (set 'load-path (cons
                           (concat
                            erlang-lib
                            (file-name-completion "tools-" erlang-lib)
                            "emacs")
                           load-path))
          (set 'load-path (cons (file-name-directory erlang-mode-path) load-path))
          (require 'erlang-start)
          (require 'erlang-eunit)
          (require 'erlang-flymake))
      (message "--> discarding the erlang-mode: %s and/or %s not readable"
               erlang-bin erlang-mode-path))))

(setq erlang-man-root-dir (expand-file-name "man" erlang-root-dir))
(setq exec-path (cons "/usr/local/opt/erlang/bin" exec-path))
(setq erlang-compile-extra-opts (list 'debug_info))

;; to calm down noise
(setq exec-path-from-shell-check-startup-files nil)
; }}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edts - Erlang Development Tool Suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; {{{ start edts
(add-hook 'after-init-hook 'edts-erlang-hook)
(defun edts-erlang-hook ()
 "Initialize EDTS."
 (require 'edts-start))
; }}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq erlang-indent-level 4)
;; (setq erlang-icr-indent t)        ;; indentation for if case receive
(setq erlang-compile-use-outdir nil) ;; makes compiling set paths each time,
                                     ;; needed for compiling in remote shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load the Distel first instead of from Wrangler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((distel-dir "/usr/local/share/distel/elisp"))
    (unless (member distel-dir load-path)
      ;; add distel-dir to the end of load-path
      (setq load-path (append load-path (list distel-dir)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrangler refactoring support for erlang                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;{{{ custom function to specify wrangler location
(defcustom erlang-wrangler-path nil
  "Location of the wrangler elisp directory."
  :group 'erlang-config
  :type 'string
  :safe 'stringp)
;}}}


; {{{ setup the erlang path for wrangler
(setq erlang-wrangler-path "/usr/local/lib/erlang/lib/wrangler-1.2.0")
(setq erlang-emacs-major-version 25) ; just to kill annoying message
; }}}

; {{{ start wrangler and load the graphviz
(when (require 'erlang-start nil t)
  (when (not (null erlang-wrangler-path))
    (add-to-list 'load-path (concat erlang-wrangler-path "/elisp"))
    (require 'wrangler))
  (add-hook 'erlang-mode-hook 'erlang-wrangler-on)
  ;; some wrangler functionalities generate a .dot file and inorder
  ;; to compile the same and view in graphviz specify the below.
  (load-file (concat erlang-wrangler-path "/elisp/graphviz-dot-mode.el")))
; }}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distel setup for erlang code auto-completion                              ;;;;
;;;; distel node connection launch with (^C-^D-n)                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; {{{ distel-node setup
(defvar inferior-erlang-prompt-timeout t) ; prevent annoying hang-on-compile
(when (locate-library "distel")
  ; distel invoked by wrangler step, so below 2 are commented out
  ; (require 'distel)
  ; (distel-setup)
  (add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
  (add-hook 'erlang-mode-hook
            '(lambda ()
               (unless erl-nodename-cache
                 (distel-load-shell))))

  (defun distel-load-shell ()
    "Load/reload the erlang shell connection to a distel node."
    (interactive)
    ;; set default distel node name
    (setq erl-nodename-cache 'distel@apple)
    (setq distel-modeline-node "distel")
    (setq derl-cookie "emacs@sampath")
    (force-mode-line-update)
    ;; Start up an inferior erlang with node name `distel'
    (let ((file-buffer (current-buffer))
          (file-window (selected-window)))
      (setq inferior-erlang-machine-options
            '("-sname" "distel"
              ; "-pz" "ebin deps/*/ebin apps/*/ebin"
              ; "-boot" "start_sasl"
              ))
      (message "--> connecting to %s" erl-nodename-cache)
      (message "--> connecting with %s" inferior-erlang-machine-options)
      (switch-to-buffer-other-window file-buffer)
      (inferior-erlang)
      (select-window file-window)
      (switch-to-buffer file-buffer))))
; }}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intellisense through esense                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;{{{ intellisense for erlang completion
(defcustom erlang-esense-path nil
  "Location of the esense directory."
  :group 'erlang-config
  :type 'string
  :safe 'stringp)

;; (setq erlang-esense-path "/opt/erlang/esense-1.12")
;; (setq load-path (cons "/opt/erlang/esense-1.12" load-path))
(setq erlang-esense-path (aqua/wild "/opt/erlang/" "esense-"))
(setq esense-distel-node "distel@apple")
(add-to-list 'load-path erlang-esense-path)
(message "--> loading and starting the esense %s" erlang-esense-path)
(require 'esense-start)
(setq esense-indexer-program
      (concat erlang-esense-path "esense.sh"))
(setq esense-completion-display-method 'frame)

;; load the above hook here
(show-paren-mode t)
;}}}

;;-------------------------------------------------------------------------------
;; use to start an erlang shell with boot flags
;;-------------------------------------------------------------------------------
(defun erl-shell (flags)
   "Start an erlang shell with FLAGS."
   (interactive (list (read-string "Flags: ")))
   (set 'inferior-erlang-machine-options (split-string flags))
   (erlang-shell))

;;-------------------------------------------------------------------------------
;; erlang options
;;-------------------------------------------------------------------------------
(defun my-erlang-mode-hook ()
  "Erlang mode specific options."
   (local-set-key [(meta l)] 'erl-find-mod)
   (local-set-key [(meta \()] 'erl-openparent)
   (local-set-key [(meta /)]  'erl-complete)
   (local-set-key [(control x) (\?)] 'erlang-man-function)
   (local-set-key [return] 'newline-and-indent)
   ;; make hack for compile command
   ;; uses Makefile if it exists, else looks for ../inc & ../ebin
   (unless (null buffer-file-name)
     (make-local-variable 'compile-command)
      (setq compile-command
            (cond ((file-exists-p "Makefile")  "make -k")
                  ((file-exists-p "../Makefile")  "make -kC..")
                  (t (concat
                      "erlc "
                      (if (file-exists-p "../ebin") "-o ../ebin " "")
                      (if (file-exists-p "../include") "-I ../include " "")
                      "+debug_info -W " buffer-file-name))))))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;-------------------------------------------------------------------------------
;; comint
;;-------------------------------------------------------------------------------
(add-hook 'comint-mode-hook 'my-comint)
(defun my-comint ()
  "Try to make the shell more like the real shell."
  (local-set-key [tab] 'comint-dynamic-complete)
  (local-set-key [(control up)] 'previous-line)
  (local-set-key [(control down)] 'next-line)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input))

(add-hook 'erlang-shell-mode-hook 'my-erlang-shell)
(defun my-erlang-shell ()
  "Option for erlang shell."
  (setq comint-dynamic-complete-functions
        '(my-erl-complete  comint-replace-by-expanded-history)))

(defun my-erl-complete ()
  "Call erl-complete if we have an Erlang node name."
  (if erl-nodename-cache
      (erl-complete erl-nodename-cache)
    nil))

;;-------------------------------------------------------------------------------
;; from https://github.com/adbl/tools-emacs/blob/master/david.emacs
;; spin up an inferior Erlang shell and remsh into the dev cluster
;;-------------------------------------------------------------------------------
(defun erlang-shell-connect-to-node (name)
  (interactive "Node name to connect to: ")
  (let* ((inferior-erlang-machine-options
          (list "-hidden"
                "-name" (format "emacs-remsh-%s" name)
                "-remsh" (format "%s@127.0.0.1" name))))
    (erlang-shell-display)))

;;-------------------------------------------------------------------------------
;; for imenu
;;-------------------------------------------------------------------------------
(defun imenu-erlang-mode-hook()
    (imenu-add-to-menubar "imenu"))
(add-hook 'erlang-mode-hook 'imenu-erlang-mode-hook)

(add-hook 'erlang-shell-mode-hook
  (lambda ()
    ;; add some Distel bindings to the Erlang shell
    (dolist (spec distel-shell-keys)
    (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;-------------------------------------------------------------------------------
;; erlang ide set-up and erlang auto-completion using auto-complete and distel
;;-------------------------------------------------------------------------------
(require 'auto-complete-distel)
(ac-config-default)
(add-to-list 'ac-sources 'auto-complete-distel)
(setq ac-auto-show-menu    0.2)
(setq ac-delay             0.2)
(setq ac-menu-height       20)
; tart auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)
(setq ac-show-menu-immediately-on-auto-complete t)
; Erlang auto-complete
(add-to-list 'ac-modes 'erlang-mode)


;{{{ auto-complete-mode so can interact with inferior erlang and
;     popup completion turn on when needed.
(add-hook 'erlang-mode-hook
          (lambda () (auto-complete-mode 1)))
;}}}

;;-------------------------------------------------------------------------------
;; erlang auto completion using company mode and distel
;;-------------------------------------------------------------------------------
(require 'company-distel)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-distel :with company-yasnippet)))

; render company's doc-buffer (default <F1> when on a completion-candidate)
; in a small popup (using popup.el) instead of showing the whole help-buffer.
(setq company-distel-popup-help t)
; specify the height of the help popup created by company
(setq company-distel-popup-height 30)
; get documentation from internet
(setq distel-completion-get-doc-from-internet t)
; Change completion symbols
(setq distel-completion-valid-syntax "a-zA-Z:_-")

;;-------------------------------------------------------------------------------
;; A number of the erlang-extended-mode key bindings are useful in the shell too
;;-------------------------------------------------------------------------------
;{{{ setup shortcut keys
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")
;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some default erlang compilation options                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;{{{ include any additional compilation options if needed
(defvar erlang-compile-extra-opts
  "Add the include directory to default compile path."
  '(bin_opt_info debug_info (i . "../include")
                            (i . "../deps")
                            (i . "../../")
                            (i . "../../../deps")))

; define where to put beam files.
(setq erlang-compile-outdir "../ebin")
;}}}

;;-------------------------------------------------------------------------------
;; flycheck support
;;-------------------------------------------------------------------------------
;{{{ flycheck with rebar3 setup
(require 'flycheck-rebar3)
(flycheck-rebar3-setup)
(flycheck-define-checker erlang
  "awesome erlang checker"
  :command ("erlc"
            "-o" temporary-directory
            (option-list "-I" flycheck-erlang-include-path)
            (option-list "-pa" flycheck-erlang-library-path)
            "-Wall"
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes erlang-mode
  :predicate (lambda ()
               (string-suffix-p ".erl" (buffer-file-name))))
(setq flycheck-erlang-include-path '("../include"))
(setq flycheck-erlang-library-path '("../_build/default/lib/*/ebin"))
(setq safe-local-variable-values
      (quote ((allout-layout . t)
              (erlang-indent-level . 4)
              (erlang-indent-level . 2))))
;}}}

;;-------------------------------------------------------------------------------
;; on the fly source code checking through flymake
;;-------------------------------------------------------------------------------
(setq flymake-log-level 3)
(setq erlang-flymake-location (concat emacs-dir "/flymake/eflymake"))


(defun flymake-erlang-init ()
  "Erlang flymake compilation settings."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                                         (file-name-directory buffer-file-name)))
         (list (concat (getenv "HOME") "/.emacs.d/flymake/eflymake"))
         (escript-exe (concat erlang-root-dir "/bin/escript"))
         (eflymake-loc (expand-file-name erlang-flymake-location)))
    (if (not (file-exists-p eflymake-loc))
        (error "Please set erlang-flymake-location to an actual location")
      (list escript-exe (list eflymake-loc local-file)))))


;;-------------------------------------------------------------------------------
;; enable flymake only for erlang mode
;;-------------------------------------------------------------------------------
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
;; (defun flymake-erlang-mode-hook ()
;;   "Set erlang flymake mode."
;;   (flymake-mode 1))
;; (add-hook 'erlang-mode-hook 'flymake-erlang-mode-hook)

;;; {{{ flymake syntax checkers

(defun flymake-syntaxerl ()
  "Erlang syntax checker for flymake."
  (flymake-compile-script-path "/opt/erlang/syntaxerl"))

(defun flymake-compile-script-path (path)
  "Syntax checker PATH for flymake."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list path (list local-file))))

(defun my-setup-erlang ()
  "Setup the syntax path for files with erlang extensions."
  (interactive)
  (unless (is-buffer-file-temp)
    (when (file-exists-p (file-truename "~/bin/syntaxerl"))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'"     flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'"     flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'"     flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'"  flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'"     flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'"  flymake-syntaxerl))
      (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))
      ;; should be the last.
      (flymake-mode 1))))

;; add the above function to erlang mode
(add-hook 'erlang-mode-hook 'my-setup-erlang)

;;; }}}


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

;;-------------------------------------------------------------------------------
;; enable flymake for rebar projects
;;-------------------------------------------------------------------------------
(defun ebm-find-rebar-top-recr (dirname)
  "Get rebar.config filename based on DIRNAME."
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
  "Find top directory of rebar project."
  (interactive)
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (project-dir (ebm-find-rebar-top-recr dirname)))
    (if project-dir
        project-dir
      (erlang-flymake-get-app-dir))))

(defun ebm-directory-dirs (dir name)
  "Find all directories in DIR with NAME."
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
  "Get the files under deps directory."
  (ebm-directory-dirs (ebm-find-rebar-top) "ebin"))

(defun ebm-get-deps-include-dirs ()
  "Include directories under deps."
  (ebm-directory-dirs (ebm-find-rebar-top) "include"))

(fset 'erlang-flymake-get-code-path-dirs 'ebm-get-deps-code-path-dirs)
(fset 'erlang-flymake-get-include-dirs-function 'ebm-get-deps-include-dirs)

;;-------------------------------------------------------------------------------
;; format the erlang records
;;-------------------------------------------------------------------------------
(defun align-erlang-record ()
  "Formatting the erlang record data structure."
  (interactive)
  (let ((from (line-beginning-position)))
    (goto-char from)
    (search-forward "-record" )
    (search-forward "{")
    (goto-char (- (point) 1))
    (ignore-errors (er/expand-region 1))
    (my-align-region-by "=")
    (goto-char from)
    (search-forward "-record" )
    (search-forward "{")
    (goto-char (- (point) 1))
    (ignore-errors (er/expand-region 1))
    (my-align-region-by "::")))


;;-------------------------------------------------------------------------------
;; prettify symbols in erlang
;;-------------------------------------------------------------------------------
(add-hook 'erlang-mode-hook 'erlang-font-lock-level-4)

(defun erlang-prettify-symbols ()
  "Prettify common erlang symbols."
  (setq prettify-symbols-alist
        '(
          ("fun" . ?ƒ)
          ("->" . ?➔)
          ("<-" . ?∈)
          ("=/=" . ?≠)
          ("=:=" . ?≡)
          ("==" . ?≈)
          ("and" . ?∧)
          ("or" . ?∨)
          (">=" . ?≥)
          ("=<" . ?≤)
          ("lists:sum" . ?∑)
          )))

(defun my-delayed-prettify ()
  "Mode is guaranteed to run after the style hooks."
  (run-with-idle-timer 0 nil (lambda () (prettify-symbols-mode 1))))
(add-hook 'erlang-mode-hook 'my-delayed-prettify)
(add-hook 'erlang-mode-hook 'erlang-prettify-symbols)

;;-------------------------------------------------------------------------------
;; which function mode for displaying function names
;;-------------------------------------------------------------------------------
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'erlang-mode))

;;-------------------------------------------------------------------------------
;; short host name same as hostname -s
;;-------------------------------------------------------------------------------
(defun short-host-name ()
  "Short host name, like `hostname -s`, remote shell likes this better."
  (interactive)
  (string-match "[^\.]+" (system-name))
  (substring (system-name) (match-beginning 0) (match-end 0)))

;;-------------------------------------------------------------------------------
;; Erlang remote shell function
;;-------------------------------------------------------------------------------
(defun erlang-shell-remote ()
  "Get the remote shell name for Erlang."
  (interactive)
  (let* ((inferior-erlang-machine-options
          (list "-sname" "emacs"
                "-remsh" (format "%s" erl-nodename-cache))))
    (erlang-shell-display)))

;;-------------------------------------------------------------------------------
;; electric commands
;;-------------------------------------------------------------------------------
; {{{ - for electric commands
;; (set-variable 'erlang-electric-commands nil) ; to disable
(setq erlang-electric-commands
      ; Insert a comma character and possibly a new indented line.
      '(erlang-electric-comma
      ; Insert a semicolon character and possibly a prototype for the next line.
        erlang-electric-semicolon
      ; Insert a '>'-sign and possible a new indented line.
        erlang-electric-gt
        ))
;}}}


;;-------------------------------------------------------------------------------
;; new file declarations
;;-------------------------------------------------------------------------------
;{{{ handling of new erlang files with header
  (defun erl-file-header ()
    "insert my very own erlang file header"
    (interactive)
    (insert "%% -*- mode: erlang; erlang-indent-level: 2 -*-\n")
    (insert "%% @doc\n")
    (insert "%% @end\n\n")
    (insert (concat "-module(" (erlang-get-module-from-file-name) ").\n\n"))
    (insert (concat "-export([]).\n\n")))

  (add-hook 'erlang-new-file-hook 'my-erlang-new-file-hook)
  (defun my-erlang-new-file-hook ()
    (erl-file-header))
;}}}

;{{{ erlang skels options
(eval-after-load "erlang-skels"
  (progn
    (setq erlang-skel-mail-address "Singamsetty.Sampath@gmail.com")))
;}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'erlang-config)
;;; erlang-config.el ends here
