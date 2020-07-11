;;; package --- Erlang Flycheck Configuration
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename: erlang-flycheck-config.el
;;; Description: Flycheck syntax checker for Emacs
;;;
;;; elisp code for erlang syntaxerl handling
;;;
;;; Code:
;;;


;;-------------------------------------------------------------------
;;** some default erlang compilation options
;;-------------------------------------------------------------------
;; include any additional compilation options if needed
(defvar erlang-compile-extra-opts
  "Add the include directory to default compile path."
  '(bin_opt_info debug_info (i . "../include")
                            (i . "../deps")
                            (i . "../../")
                            (i . "../../../deps")))

; define where to put beam files.
(setq erlang-compile-outdir "../ebin")

;;-------------------------------------------------------------------
;;** [FlyCheck] - Real Time syntax checking support through rebar3
;;-------------------------------------------------------------------
(after "flycheck"
  (require 'flycheck-rebar3)
  (flycheck-rebar3-setup)

  (flycheck-define-checker erlang
    "awesome erlang checker."
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
                (erlang-indent-level . 2)))))

;;-------------------------------------------------------------------
;; [FlyMake] - source code syntax checking for erlang
;;-------------------------------------------------------------------
(after "flymake"
  (require 'erlang-flymake)

  (setq flymake-log-level 3)
  (setq erlang-flymake-location (concat user-emacs-directory "flymake/eflymake"))

  (defun flymake-create-temp-intemp (file-name prefix)
    "Return file name in temporary directory for checking FILE-NAME.
  This is a replacement for `flymake-create-temp-inplace'. The
  difference is that it gives a file name in
  `temporary-file-directory' instead of the same directory as
  FILE-NAME.
  For the use of PREFIX see that function.
  Note that not making the temporary file in another directory
  \(like here) will not if the file you are checking depends on
  relative paths to other files \(for the type of checks flymake
  makes)."
    (unless (stringp file-name)
      (error "Invalid file-name"))
    (or prefix
        (setq prefix "flymake"))
    (let* ((name (concat
                  (file-name-nondirectory
                   (file-name-sans-extension file-name))
                  "_" prefix))
           (ext  (concat "." (file-name-extension file-name)))
           (temp-name (make-temp-file name nil ext))
           )
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

  (defun flymake-erlang-init ()
    "Initialize the erlang flymake temp buffer."
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/flymake/eflymake" (list local-file))))

  (defun init-flymake ()
    "Initialize the FlyMake mode for Erlang."
  	(when (locate-library "flymake")
  		(require 'flymake)
  		(add-to-list 'flymake-allowed-file-name-masks
  								 '("\\.erl\\'" flymake-erlang-init))
  		(flymake-mode 1)))

  (add-hook 'erlang-mode-hook 'init-flymake)

  ;;** flymake syntax checkers
  (defun flymake-syntaxerl ()
    "Erlang syntax checker for flymake."
    (flymake-compile-script-path "/opt/erlang/syntaxerl/syntaxerl"))

  (defun flymake-compile-script-path (path)
    "Syntax checker PATH for flymake."
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                        'flymake-create-temp-inplace))
            (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
      (list path (list local-file))))

  (defun my-setup-erlang ()
    "Erlang syntax checker with syntaxerl."
    (interactive)
    (unless (is-buffer-file-temp)
      (when (file-exists-p (file-truename "/opt/erlang/syntaxerl/syntaxerl"))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'" flymake-syntaxerl))
        (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))
        ;; should be the last.
        (flymake-mode 1))))

    ;; add the above function to erlang mode
    ;; having conflicts with distel and edts
    ;;(add-hook 'erlang-mode-hook 'my-setup-erlang)
)


(provide 'erlang-flycheck-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; erlang-flycheck-config.el ends here
