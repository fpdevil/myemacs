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
;;;===========================================================================
(require 'ensime)               ; ensime loads scala-mode2 internally
(require 'sbt-mode)             ; interact with scala and sbt projects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENhanced Scala Interaction Mode for Emacs (for scala development)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(setq ensime-startup-snapshot-notification nil)
(setq ensime-completion-style 'company
      ensime-graphical-tooltips t
      ensime-auto-generate-config t)

(after "company"
 (setq company-minimum-prefix-length 1))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq ensime-sbt-command (executable-find "sbt")
      sbt:program-name (executable-find "sbt"))

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

(add-hook 'scala-mode-hook
  (lambda ()
    (ensime-scala-mode-hook)
    (setq prettify-symbols-alist scala-prettify-symbols)
    (prettify-symbols-mode)
    (define-key scala-mode-map (kbd "C-x M-e") 'ensime-fully-reload)
  ))

(setq ensime-sem-high-faces
      '(
        (implicitConversion nil)
        (var . (:foreground "#ff2222"))
        (val . (:foreground "#dddddd"))
        (varField . (:foreground "#ff3333"))
        (valField . (:foreground "#dddddd"))
        (functionCall . (:foreground "#dc9157"))
        (param . (:foreground "#ffffff"))
        (object . (:foreground "#D884E3"))
        (class . (:foreground "green"))
        (trait . (:foreground "#009933")) ;; "#084EA8"))
        (operator . (:foreground "#cc7832"))
        (object . (:foreground "#6897bb" :slant italic))
        (package . (:foreground "yellow"))
        (implicitConversion . (:underline (:style wave :color "blue")))
        (implicitParams . (:underline (:style wave :color "blue")))
        (deprecated . (:strike-through "#a9b7c6"))
        (implicitParams nil))
        ;; ensime-completion-style 'company
        ;; ensime-sem-high-enabled-p nil ;; disable semantic highlighting
        ensime-tooltip-hints t ;; disable type-inspecting tooltips
        ensime-tooltip-type-hints t ;; disable typeinspecting tooltips
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensime                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ensime-auto-start ()
  "Start Ensime."
  (interactive)
  (unless (get-buffer-process "*ENSIME*")
    (ensime)))

;; if auto starting of ensime is needed
;; (add-hook 'ensime-mode-hook #'ensime-auto-start)

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
  (sbt-command "test:compile"))

(add-hook 'scala-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'compile-sbt-project)))

;;------------------------------------------------------------------------------

(provide 'scala-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; scala-config.el ends here
