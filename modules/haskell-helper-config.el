;;; package --- helper configuration settings for Haskell
;;;
;;; Commentary:
;;;
;;; Filename   : haskell-helper-config.el
;;; Description: Haskell Auto Insert doc strings and any other...
;;;              Haskell syntax checking with flymake & flycheck
;;;
;;; elisp code for haskell helpers
;;;
;;; Code:
;;;
;;==============================================================================

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; flcheck handler for checking Haskell source code with hlint.
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(require 'flycheck-haskell)           ;; improved flycheck support for Haskell
(after 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(setq flycheck-haskell-hlint-executable (executable-find "hlint"))

;{{{
; (after 'flycheck
;   '(progn
;      ;; (flycheck-mode)
;      (setq flycheck-haskell-hlint-executable (executable-find "hlint"))
;      (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
;      ;; (setq flycheck-check-syntax-automatically '(save))
;      (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
;      (add-hook 'haskell-mode-hook 'flycheck-mode)
;      (add-hook 'haskell-mode-hook
;                '(lambda ()
;                   (setq flycheck-checker 'haskell-hlint)
;                   (setq flycheck-disabled-checkers '(haskell-ghc))))))
;}}}

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; flymake handler for checking Haskell source code with hlint.
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(require 'flymake-hlint)              ;; flymake handler for checking Haskell source with hlint
(add-hook 'haskell-mode-hook 'flymake-hlint-load)

;; haskell flymake configuration
(when (load "flymake" t)
  (defun flymake-hslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/flycheck/hslint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.hs$\\'" flymake-hslint-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.lhs$\\'" flymake-hslint-init)))

;; {{{
;; flymake-mode for Haskell with the Perl script flymake_haskell.pl
; (when (load "flymake" t)
;   ;; (add-hook 'haskell-mode-hook 'flymake-hlint-load)
;   (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
;   (defun flymake-haskell-init (&optional trigger-type)
;     "Return the command to run Python checks with pyflymake.py"
;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                        'flymake-create-temp-inplace))
;            (local-file (file-relative-name
;                         temp-file
;                         (file-name-directory buffer-file-name)))
;            (options (when trigger-type (list "--trigger-type" trigger-type))))
;       ;; after an extended check, disable check-on-edit
;       (when (member trigger-type '("save" "force"))
;         (setq flymake-no-changes-timeout 18600))
;       (list "~/.emacs.d/flymake/flymake_haskell.pl" (append options (list local-file)))))

;   (push '(".+\\hs$" flymake-haskell-init) flymake-allowed-file-name-masks)
;   (push '(".+\\lhs$" flymake-haskell-init) flymake-allowed-file-name-masks)
;   (push
;    '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;      1 2 3 4) flymake-err-line-patterns))
; }}}

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; DOC Strings
;; auto insert haskell module header when a new file is created
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(require 'skeleton)
(require 'autoinsert)

;; -- A Haskell module definition template (inserted first time when opened)
(define-skeleton haskell-module-skeleton
  "Haskell source module header."
  "Brief description (can be left blank for defaults):"
  "-----------------------------------------------------------------------------\n"
  "-- \|\n"
  "-- Module      : " (setq v1 (or (haskell-guess-module-name) "Main"))"\n"
  "-- Copyright   : " (haskell-cabal-guess-setting "copyright") | (concat "© " user-full-name)"\n"
  "--\n"
  "-- License     : " (haskell-cabal-guess-setting "license") | "BSD-style (see the file LICENSE)""\n"
  "-- Author      : " (user-full-name)"\n"
  "-- Maintainer  : " (haskell-cabal-guess-setting "maintainer") | user-mail-address"\n"
  "-- Description : " str | (concat "The \\\"" v1 "\\\" module")"\n"
  "--\n"
  "   "_"\n"
  "\n"
  "--\n"
  "module " v1 " where\n\n")

;; un-comment the below line to insert module docstring as defined in haskell-module-skeleton
;; (add-to-list 'auto-insert-alist '("\\.hs\\|.lhs\\'" . haskell-module-skeleton))

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; -- a simple module level document string insertion
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(defun haskell-auto-insert-module-template ()
  "Insert a simple module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)))

;; in order to use the above to insert doc string, uncomment below
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; insert comment doc with C-c C-a
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (unless (= (line-beginning-position)
             (line-end-position))
    (shm/backward-paragraph))
  (unless (= (line-beginning-position)
             (line-end-position))
    (save-excursion (insert "\n")))
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; indentation settings
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; A function used to change the behaviour of return.  If you press it at
;; the start of a line, it will just move the code down a line.  If you
;; press it anywhere else, you get automatic indentation.
;; (defun haskell-ret()
;;   "Return"
;;   (interactive)
;;   (if (bolp) (newline) (newline-and-indent)))
;; (add-hook 'haskell-mode-hook (lambda () (local-set-key (kbd "RET") 'haskell-ret)))


(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
;;(add-hook 'haskell-mode-hook 'set-newline-and-indent)

;; set tab width to 2
(add-hook 'haskell-mode-hook
          (lambda ()
            ;; use spaces instead of tabs when indenting
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq whitespace-tab-width 2)))

;;------------------------------------------------------------------------------
;; intero (not used, so commented)
;;------------------------------------------------------------------------------
; (require-package 'intero)
; (require 'intero)                   ;; complete development mode for haskell
; (defun haskell-process-cabal-build-and-restart ()
;   "Build and restart the Cabal project."
;   (interactive)
;   (intero-devel-reload))
;
; (add-hook 'haskell-mode-hook 'intero-mode)
; ;; key map
; (define-key intero-mode-map (kbd "C-`") 'flycheck-list-errors)
; (define-key intero-mode-map [f12] 'intero-devel-reload)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'haskell-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; haskell-helper-config.el ends here
