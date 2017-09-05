;;; Erlang Flycheck Configuration
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename: erlang-flycheck-config.el
;;; Description: Flycheck syntax checker for Emacs
;;;
;;; elisp code for erlang syntaxerl handling
;;=================================================================================

;;;
;;; Code:
;;;

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
          (list "~/.emacs.d/flymake/eflymake")
          (escript-exe (concat erlang-root-dir "/bin/escript"))
          (eflymake-loc (expand-file-name erlang-flymake-location)))
    (if (not (file-exists-p eflymake-loc))
      (error "Please set erlang-flymake-location to an actual location")
      (list escript-exe (list eflymake-loc local-file)))))

;;-------------------------------------------------------------------------------
;; enable flymake only for erlang mode
;;-------------------------------------------------------------------------------
; {{{ flymake syntax checkers
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

(defun my-erlang-setup ()
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
(add-hook 'erlang-mode-hook 'my-erlang-setup)
; }}}

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
;; new file declarations
;;-------------------------------------------------------------------------------
;{{{ handling of new erlang files with header

(defun erl-file-header ()
  "Insert a custom edoc header at the top."
  (interactive)
  (save-excursion
    (when (re-search-forward "^\\s *-spec\\s +\\([a-zA-Z0-9_]+\\)\\s *(\\(\\(.\\|\n\\)*?\\))\\s *->[ \t\n]*\\(.+?\\)\\." nil t)
      (let* ((beg (match-beginning 0))
             (funcname (match-string-no-properties 1))
             (arg-string (match-string-no-properties 2))
             (retval (match-string-no-properties 4))
             (args (split-string arg-string "[ \t\n,]" t)))
        (when (re-search-forward (concat "^\\s *" funcname "\\s *(\\(\\(.\\|\n\\)*?\\))\\s *->") nil t)
          (let ((arg-types (split-string (match-string-no-properties 1) "[ \t\n,]" t)))
            (goto-char beg)
            (insert "%%-----------------------------------------------------------------------------\n")
            (insert "%% @doc\n")
            (insert "%% Your description goes here\n")
            (insert "%% @spec " funcname "(")
            (dolist (arg args)
              (if (string-match "::" arg) (insert arg) (insert (car arg-types) "::" arg))
              (setq arg-types (cdr arg-types))
              (when arg-types
                (insert ", ")))
            (insert ") ->\n")
            (insert "%%       " retval "\n")
            (insert "%% @end\n")
            (insert "%%-----------------------------------------------------------------------------\n")))))))

;}}}

;{{{ erlang skels options
(eval-after-load "erlang-skels"
  (progn
    (setq erlang-skel-mail-address "Singamsetty.Sampath@gmail.com")))
;}}}

;;-------------------------------------------------------------------------------
;; electric commands
;;-------------------------------------------------------------------------------
; {{{ - for electric commands
;; (set-variable 'erlang-electric-commands nil) ; to disable
(setq erlang-electric-commands
      ;; Insert a comma character and possibly a new indented line.
      '(erlang-electric-comma
        ;; Insert a semicolon character and possibly a prototype for the next line.
        erlang-electric-semicolon
        ;; Insert a '>'-sign and possible a new indented line.
        erlang-electric-gt
        ))
;}}}

(message "loading the erlang syntex checking...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'erlang-flycheck-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; erlang-flycheck-config.el ends here
