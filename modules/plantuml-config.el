;;; package --- plantuml configuration
;;;
;;; Commentary:
;;;
;;; Filename   : plantuml-config.el
;;; Description: A major mode for editing PlantUML sources and Ditaa in Emacs
;;;              https://github.com/skuro/plantuml-mode
;;;              http://archive.3zso.com/archives/plantuml-quickstart.html
;;;              -- ditaa --
;;;              http://ditaa.sourceforge.net/#usage
;;;              https://github.com/stathissideris/ditaa
;;;              -- http --
;;;              https://github.com/zweifisch/ob-http
;;;
;;; elisp code for org support and handling
;;;
;;; Code:
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;;; plantuml configuration
;;-----------------------------------------------------------------------------
;; tell org-mode where to find the plantuml JAR file
;; (setq org-plantuml-jar-path (concat user-emacs-directory "/vendor/java/plantuml.jar")
;;       plantuml-jar-path (concat user-emacs-directory "/vendor/java/plantuml.jar"))
(setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"
      plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
(message "--> loading plantuml from %s" org-plantuml-jar-path)

;;-----------------------------------------------------------------------------
;; ditaa - DIagrams Through Ascii Art | brew install ditaa
;;-----------------------------------------------------------------------------
;; Location where homebrew installed the ditaa
(setq org-ditaa-jar-path "/usr/local/opt/ditaa/libexec/ditaa0_10.jar")
(setq ditaa-cmd "java -jar /usr/local/opt/ditaa/libexec/ditaa0_10.jar")
(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
    (concat ditaa-cmd " " buffer-file-name)))

;;-----------------------------------------------------------------------------
;;; activate Babel language and use plantuml as org-babel language
;;-----------------------------------------------------------------------------
(when (boundp 'org-plantuml-jar-path)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org        . t)
     (latex      . t)
     (haskell    . t)
     (emacs-lisp . t)
     (scala      . t)
     (js         . t)
     (shell      . t)
     ;; (sh      . t)
     (python     . t)
     (dot        . t)
     (plantuml   . t)
     (ditaa      . t)
     (http       . t))))

(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; for line truncation
(defun my-org-mode-hook ()
  (toggle-truncate-lines t))
(add-hook 'org-mode-hook #'my-org-mode-hook)

;;-----------------------------------------------------------------------------
;; Asynchronous src_block execution for org-babel
;;-----------------------------------------------------------------------------
(require-package 'ob-async)
(add-to-list 'org-ctrl-c-ctrl-c-hook #'ob-async-org-babel-execute-src-block)

;;-----------------------------------------------------------------------------
;; make dot work as graphviz-dot
;;-----------------------------------------------------------------------------
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;;-----------------------------------------------------------------------------
;; integration with org-mode
;; use plantuml-mode to edit PlantUML source snippets within org-mode docs
;;-----------------------------------------------------------------------------
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;;-----------------------------------------------------------------------------
;; auxiliary function
;;-----------------------------------------------------------------------------
(defun my-org-confirm-babel-evaluate (lang body)
  "LANG BODY Do not ask for confirmation to evaluate code for specified languages."
  (member lang '("plantuml")))

;;-----------------------------------------------------------------------------
;; trust certain code as being safe
;;-----------------------------------------------------------------------------
(defun aqua-display-inline-images ()
  "For displaying images inline."
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'aqua-display-inline-images 'append)

;;-----------------------------------------------------------------------------
;; flycheck integration
;;-----------------------------------------------------------------------------
(after 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'plantuml-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; plantuml-config.el ends here
