;;; package  --- scala-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : scala-config.el
;;; Description: Emacs configuration for Scala with Ensime
;;;              ENhanced Scala Interaction Mode for Emacs
;;;
;;; elisp code for customizing the scala development settings
;;;
;;; Code:
;;;

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; interact with scala and sbt projects
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Optional - enable lsp-mode automatically in scala files
(add-hook 'scala-mode-hook #'lsp)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)


;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala pretty fonts                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom scala-prettify-symbols nil
  "Define custom symbols for scala mode.")

(setq scala-prettify-symbols
  '(
    ("=>"        . ?⇒)
    ("<-"        . ?←)
    ("->"        . ?→)
    ("undefined" . ?⊥)
    ("&&"        . ?∧)
    ("||"        . ?∨)
    ("<<<"       . ?⋘)
    (">>>"       . ?⋙)
    ("++"        . ?⧺)
    ("any"       . ?∃)
    ("all"       . ?∀)
    ("traverse"  . ?↦)
    ("map"       . ?∘)
    ("lambda"    . ?λ)
    ("alpha"     . ?α)
    ("beta"      . ?β)
    ("Unit"      . ?∅)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala and play                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun play-start ()
  "Start Play console."
  (interactive)
  (let ((sbt:program-name "play")) ; do a 'let' binding for the variable
    (sbt-start)))
(setq scala-indent:use-javadoc-style t)

(defun test-only ()
  "Run test with current file."
  (interactive)
  (sbt-command (concat "testOnly " (find-spec-name))))

(defun find-spec-name ()
  "Find spec name of current buffer."
  (concat "*." (file-name-sans-extension (file-name-nondirectory (buffer-name)))))

(defun compile-sbt-project ()
  "Compile the sbt project."
  (sbt-command "test:compile"))

(add-hook 'scala-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'compile-sbt-project)))


(provide 'scala-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; scala-config.el ends here
