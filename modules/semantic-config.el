;;; package --- semantic configuration settings for c/c++
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : semantic-config.el
;;; Description: Semantic is a suite of Emacs libraries and utilities
;;;              for parsing source code
;;;
;;; elisp code for customizing the semantic mode for parsing c/c++ code
;;; reference https://github.com/AndreaOrru/emacs.d and Emacs manual
;;;
;;; Code:
;;;
;;;=============================================================================
(require 'semantic)
(require 'semantic/ia)

;;------------------------------------------------------------------------------
;; Project Management with EDE - turn on EDE only in C/C++
;;------------------------------------------------------------------------------
;; (require 'ede)
;; (global-ede-mode)           ;; turn on ede mode

;; specify the location for project file
;; (after 'ede
;;   (setq semanticdb-default-save-directory
;;         (expand-file-name "ede-projects.el" cache-dir))
;;   (setq ede-simple-save-directory
;;         (expand-file-name "EDE" cache-dir)))

;;------------------------------------------------------------------------------
;; CEDET
;;------------------------------------------------------------------------------
(require 'cedet)
(require 'cedet-files)
(setq semantic-load-turn-useful-things-on t)
(defun aqua/cedet-hook ()
 (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
 (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'aqua/cedet-hook)
(add-hook 'c-mode-hook 'aqua/cedet-hook)
(add-hook 'c++-mode-hook 'aqua/cedet-hook)

;;------------------------------------------------------------------------------
;; activate semantic for c & c++ modes
;;------------------------------------------------------------------------------
(setq semantic-default-submodes
      '(;; Perform semantic actions during idle time
        global-semantic-idle-scheduler-mode
        ;; Use a database of parsed tags
        global-semanticdb-minor-mode
        ;; Decorate buffers with additional semantic information
        global-semantic-decoration-mode
        ;; Highlight the name of the function you're currently in
        global-semantic-highlight-func-mode
        ;; show the name of the function at the top in a sticky
        global-semantic-stickyfunc-mode
        ;; Generate a summary of the current tag when idle
        global-semantic-idle-summary-mode
        ;; Show a breadcrumb of location during idle time
        global-semantic-idle-breadcrumbs-mode
        ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
        ;; or `C-x B'
        global-semantic-mru-bookmark-mode))

;; (semantic-mode 1)
(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c++-mode-hook 'semantic-mode)

(after "semantic"

  ;; disable Semantic for everything but C/C++:
  (add-to-list 'semantic-inhibit-functions
               (lambda()
                 (not (member major-mode '(c-mode c++-mode)))))

  ;; disable semantic mode for python
  (add-hook 'semantic-inhibit-functions
            (lambda () (member major-mode '(python-mode))))

  ;;------------------------------------------------------------------------------
  ;; define a function which adds semantic as a suggestion backend for  auto
  ;; completion ac hook that function to the c-mode-common-hook
  ;;------------------------------------------------------------------------------
  (after "auto-complete"
    (defun my:add-semantic-to-autocomplate()
      "Sematic mode hook."
      (add-to-list 'ac-sources 'ac-source-semantic))
    (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplate))

  ;; specify the location for semantic db
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb" cache-dir))
  (when (featurep 'semanticdb)
    (setq semanticdb-default-save-directory
          (expand-file-name "semanticdb" cache-dir))
    (when (not (file-exists-p semanticdb-default-save-directory))
      (make-directory semanticdb-default-save-directory)))

  ;; Try to make completions when not typing
  '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
  '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-scheduler-mode 1) ;The idle scheduler with automatically reparse buffers in idle time.
  (global-semantic-idle-summary-mode 1)

  ;;------------------------------------------------------------------------------
  ;; By default, Semantic automatically includes some default system include
  ;; paths such as /usr/include, /usr/local/include. Specify any additional ones
  ;;------------------------------------------------------------------------------
  ;; (semantic-add-system-include "/usr/include" 'c++-mode)
  ;; (semantic-add-system-include "/usr/local/include" 'c++-mode)
  ;; (semantic-add-system-include "/usr/local/opt/opencv3/include" 'c++-mode)
  ;; (semantic-add-system-include "/usr/local/opt/opencv3/include/opencv" 'c++-mode)
  ;; (semantic-add-system-include "/usr/local/opt;/opencv3/include/opencv2" 'c++-mode)

  ;;------------------------------------------------------------------------------
  ;; Prohibit semantic from searching through system headers. We want
  ;; company-clang to do that for us.
  ;;------------------------------------------------------------------------------
  (setq-mode-local c-mode semanticdb-find-default-throttle '(local project unloaded recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle '(local project unloaded recursive))

  (semantic-remove-system-include "/usr/include/" 'c++-mode)
  (semantic-remove-system-include "/usr/local/include/" 'c++-mode)
  (add-hook 'semantic-init-hooks 'semantic-reset-system-include)

  ;;------------------------------------------------------------------------------
  ;; Disable tag boundary decoration:
  ;;------------------------------------------------------------------------------
  (add-hook 'semantic-mode-hook
            '(lambda()
               (semantic-toggle-decoration-style
                "semantic-tag-boundary" -1)))

  ;; -- integration with imenu
  (defun my-semantic-hook ()
    (imenu-add-to-menubar "TAGS"))
  (add-hook 'semantic-init-hooks 'my-semantic-hook)

  ;;------------------------------------------------------------------------------
  ;; Use info extracted from Irony mode to help Semantic
  ;;------------------------------------------------------------------------------
  (defadvice irony-cdb--update-compile-options (after my/irony-cdb--update-compile-options activate)
    "Pass the include paths found by Irony to Semantic."
    (dolist (dir (irony--extract-user-search-paths
                  irony--compile-options
                  irony--working-directory))
      (semantic-add-system-include dir)))

  ;;------------------------------------------------------------------------------
  ;; force reloading all the includes after Irony update
  ;;------------------------------------------------------------------------------
  (add-to-list 'semantic-init-db-hook
               (lambda ()
                 (semanticdb-find-translate-path nil nil)))
)



;;------------------------------------------------------------------------------
;; semantic parsing
;;------------------------------------------------------------------------------
(defun semantic-parse-dir (root)
 "Make Semantic parse all the source files in ROOT directory, recursively."
 (interactive
  (list (read-directory-name "Root directory; "
                             irony--working-directory)))
 (dolist (file (directory-files-recursively
                root
                ".*\\.\\(c\\|cpp\\|cxx\\|h\\|hpp\\|hxx\\)"))
   (semanticdb-file-table-object file)))

;;------------------------------------------------------------------------------
;; disable and enable company-semantic backend at will
;;------------------------------------------------------------------------------
(defun my-disable-semantic ()
 "Disable the company-semantic backends."
 (interactive)
 (when (boundp 'company-backends)
   (make-local-variable 'company-backends)
   (setq company-backends  (delete '(company-irony-c-headers
                                     company-irony
                                     company-yasnippet
                                     company-clang
                                     ;; company-rtags
                                     company-semantic)
                                   company-backends))
   (add-to-list 'company-backends '(company-irony
                                    company-irony-c-headers
                                    company-yasnippet
                                    ;; company-rtags
                                    company-clang))))

(defun my-enable-semantic ()
 "Enable the company-semantic backends."
 (interactive)
 (when (boundp 'company-backends)
   (make-local-variable 'company-backends)
   (setq company-backends (delete '(company-irony-c-headers
                                    company-irony
                                    company-yasnippet
                                    company-clang)
                                  company-backends))
   (add-to-list 'company-backends '(company-irony-c-headers
                                    company-irony
                                    company-yasnippet
                                    company-clang))))

;;------------------------------------------------------------------------------
;; jump to the definition of the symbol at point for SemanticBovinator scanned code
;;------------------------------------------------------------------------------
(defun find-definition (arg)
 "ARG Jump to the definition of the symbol, type or function at point.
With prefix arg, find in other window."
 (interactive "P")
 (let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
                 (semantic-idle-summary-current-symbol-info-brutish)
                 (error "No known tag at point")))
        (pos (or (semantic-tag-start tag)
                 (error "Tag definition not found")))
        (file (semantic-tag-file-name tag)))
   (if file
       (if arg (find-file-other-window file) (find-file file))
     (if arg (switch-to-buffer-other-window (current-buffer))))
   (push-mark)
   (goto-char pos)
   (end-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'semantic-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; semantic-config.el ends here
