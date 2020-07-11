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

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; flcheck handler for checking Haskell source code with hlint.
;; improved flycheck support for Haskell
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(after "flycheck"
  (require 'flycheck-haskell)
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  (setq flycheck-haskell-hlint-executable (executable-find "hlint")))

;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;; flymake handler for checking Haskell source code with hlint.
;; flymake-hlint is handler for checking Haskell source with hlint
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(after "flymake"
  (require 'flymake-hlint)
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
    (add-to-list 'flymake-allowed-file-name-masks '("\\.lhs$\\'" flymake-hslint-init))))


;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
;;**  DOC Strings
;;*** auto insert haskell module header when a new file is created
;;== == == == == == == == == == == == == == == == == == == == == == == == == ==
(require 'skeleton)
(require 'autoinsert)

;;** A Haskell module definition template (inserted first time when opened)
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
;;** a simple module level document string insertion
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
;;** insert comment doc with C-c C-a
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
;;** indentation settings
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
;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil) ;; use spaces instead of tabs when indenting
;;             (setq tab-width 2)
;;             (setq whitespace-tab-width 2)))


;; Get Haskell indentation which mirrors what I'm used to from Vim
(defun haskell-indent-setup ()
  "Setup variables for editing Haskell files."
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (number-sequence 0 120 4))
  (setq indent-line-function 'tab-to-tab-stop)

  ; Backspace: delete spaces up until a tab stop
  (defvar my-offset 4 "My indentation offset. ")
  (defun backspace-whitespace-to-tab-stop ()
    "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
    (interactive)
      (let ((movement (% (current-column) my-offset))
            (p (point)))
        (when (= movement 0) (setq movement my-offset))
        ;; Account for edge case near beginning of buffer
        (setq movement (min (- p 1) movement))
        (save-match-data
          (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
              (backward-delete-char (- (match-end 1) (match-beginning 1)))
            (call-interactively 'backward-delete-char)))))

  (local-set-key (kbd "DEL") 'backspace-whitespace-to-tab-stop))
(add-hook 'haskell-mode-hook 'haskell-indent-setup)


;;------------------------------------------------------------------------------
;;** alignment rules for haskell
;;------------------------------------------------------------------------------
(eval-after-load 'align
  '(nconc
    align-rules-list
    (mapcar #'(lambda (x)
                `(,(car x)
                  (regexp . ,(cdr x))
                  (modes quote (haskell-mode literate-haskell-mode))))
            '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
              (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
              (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
              (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))))

;; Setup haskell-doc
(use-package haskell-doc
  :diminish haskell-doc-mode
  :config
  (setq haskell-doc-show-global-types nil
        haskell-doc-show-reserved     t
        haskell-doc-show-prelude      nil ;;t
        haskell-doc-show-strategy     t
        haskell-doc-show-user-defined t
        haskell-doc-chop-off-context  nil ;; t
        haskell-doc-chop-off-fctname  nil
        haskell-doc-prettify-types    nil)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode))


(provide 'haskell-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; haskell-helper-config.el ends here
