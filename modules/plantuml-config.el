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
;;; Real worl examples @https://real-world-plantuml.com/
;;;
;;; elisp code for org support and handling
;;; examples @href http://plantuml.com/
;;;
;;; Code:
;;;
;;;============================================================================

;; file mode enable
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.uml\\'"      . plantuml-mode))

;; disable electric indent mode
(add-hook 'electric-indent-mode-hook (lambda () (electric-indent-local-mode -1)))
;; to restore
(add-hook 'electric-indent-mode-hook
          (lambda () (local-set-key (kbd "C-j") #'newline-and-indent)))

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
;;; activate Babel language and use plantuml as org-babel language
;;-----------------------------------------------------------------------------
(when (boundp 'org-plantuml-jar-path)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org        . t)
     (latex      . t)
     (haskell    . t)
     (elixir     . t)
     (coq        . t)
     (js         . t)
     (emacs-lisp . t)
     (scala      . t)
     (js         . t)
     (java       . t)
     (shell      . t)
     (sql        . t)
     (http       . t)
     ;; (sh      . t)
     (python     . t)
     (ipython    . t)
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
;; integration with org-mode
;; use plantuml-mode to edit PlantUML source snippets within org-mode docs
;; #+BEGIN_SRC plantuml
;;   <hit C-c ' here to open a plantuml-mode buffer>
;; #+END_SRC
;;-----------------------------------------------------------------------------
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;;-----------------------------------------------------------------------------
;; auxiliary function
;;-----------------------------------------------------------------------------
(defun my-org-confirm-babel-evaluate (lang body)
  "LANG BODY Do not ask for confirmation to evaluate code for specified languages."
  (member lang '("plantuml")))

;; trust certain code as being safe
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;;-----------------------------------------------------------------------------
;; trust certain code as being safe | displaying inline images
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

;;------------------------------------------------------------------------------
;; Save png image in plantuml-mode
;;------------------------------------------------------------------------------
;; If you want to save png file while saving .plantuml file, comment in here
;; (add-hook 'plantuml-mode-hook
;;    (lambda () (add-hook 'after-save-hook 'plantuml-save-png)))
(defun plantuml-save-png ()
  "Save the plantuml image."
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
      'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
         out-file
         cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
                "java -Djava.awt.headless=true -jar " plantuml-java-options " "
                (shell-quote-argument plantuml-jar-path) " "
                (and out-file (concat "-t" (file-name-extension out-file))) " "
                plantuml-options " "
                (buffer-file-name)))
    (message cmd)
    (call-process-shell-command cmd nil 0)))

;;-----------------------------------------------------------------------------
;; ditaa - DIagrams Through Ascii Art | brew install ditaa
;;-----------------------------------------------------------------------------
;; Location where homebrew installed the ditaa
(setq org-ditaa-jar-path "/usr/local/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar")
(setq ditaa-cmd "java -jar /usr/local/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar")
(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
    (concat ditaa-cmd " " buffer-file-name)))

;;-----------------------------------------------------------------------------
;; make dot work as graphviz-dot
;;-----------------------------------------------------------------------------
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'plantuml-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; plantuml-config.el ends here
