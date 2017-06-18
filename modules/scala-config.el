;;; package  --- scala-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : scala-config.el
;;; Description: Emacs configuration for Scala with Ensime
;;;              ENhanced Scala Interaction Mode for Emacs
;;;
;;; elisp code for customizing the scala development settings
;;;===========================================================================
(require 'ensime)               ; ensime loads scala-mode2 internally
(require 'sbt-mode)             ; interact with scala and sbt projects
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENhanced Scala Interaction Mode for Emacs (for scala development)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(setq ensime-startup-snapshot-notification nil)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq
  ensime-sbt-command "/usr/local/bin/sbt"
  sbt:program-name "/usr/local/bin/sbt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala pretty fonts                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom scala-prettify-symbols nil
  "Define custom symbols for scala mode.")

(setq scala-prettify-symbols
  '(
    ("=>" . ?⇒)
    ("<-" . ?←)
    ("->" . ?→)
    ("undefined" . ?⊥)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("<<<" . ?⋘)
    (">>>" . ?⋙)
    ("++" . ?⧺)
    ("any" . ?∃)
    ("all" . ?∀)
    ("traverse" . ?↦)
    ("map" . ?∘)
    ("lambda" . ?λ)
    ("alpha" . ?α)
    ("beta" . ?β)
    ("Unit" . ?∅)
  ))

(add-hook 'scala-mode-hook
  (lambda ()
    (ensime-scala-mode-hook)
    (setq prettify-symbols-alist scala-prettify-symbols)
    (prettify-symbols-mode)
    (define-key scala-mode-map (kbd "C-x M-e") 'ensime-fully-reload)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensime                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ensime-fully-reload ()
  "Reload Ensime."
  (interactive)
  (ensime-shutdown)
  (ensime))

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
  (sbt-command "test:compile")
  )

(add-hook 'scala-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'compile-sbt-project)))

;;---------------------------------------------------------------------------

(provide 'scala-config)

;;; scala-config.el ends here
