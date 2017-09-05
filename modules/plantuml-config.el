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
;;;==========================================================================
(require 'org)
;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plantuml configuration                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tell org-mode where to find the plantuml JAR file
(setq org-plantuml-jar-path (concat user-emacs-directory "/vendor/java/plantuml.jar")
      plantuml-jar-path (concat user-emacs-directory "/vendor/java/plantuml.jar"))
(message "--> loading plantuml from %s" org-plantuml-jar-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ditaa - DIagrams Through Ascii Art | brew install ditaa                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Location where homebrew installed the ditaa
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.10/libexec/ditaa0_10.jar")
(setq ditaa-cmd "java -jar /usr/local/Cellar/ditaa/0.10/libexec/ditaa0_10.jar")
(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
    (concat ditaa-cmd " " buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; activate Babel language and use plantuml as org-babel language         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '((org . t)
   (latex . t)
   (haskell . t)
   (emacs-lisp . t)
   (scala . t)
   (js . t)
   (shell . t)
   ;(sh . t)
   (python . t)
   (dot . t)
   (plantuml . t)
   (ditaa . t)
   (http . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make dot work as graphviz-dot                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration with org-mode                                                ;;
;; use plantuml-mode to edit PlantUML source snippets within org-mode docs  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auxiliary function                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-org-confirm-babel-evaluate (lang body)
  "LANG BODY Do not ask for confirmation to evaluate code for specified languages."
  (member lang '("plantuml")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trust certain code as being safe                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aqua-display-inline-images ()
  "For displaying images inline."
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'aqua-display-inline-images 'append)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck integration                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'plantuml-config)
;;; plantuml-config.el ends here
