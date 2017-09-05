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
;;===========================================================================
(require 'semantic)
(require 'irony)
;
;;; Code:
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; turn on Semantic                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; turn on ede mode
(global-ede-mode 1)
; activate semantic
; (semantic-mode 1)
; Disable Semantic for everything but C/C++:
(with-eval-after-load 'semantic
  (add-to-list 'semantic-inhibit-functions
               (lambda()
                 (not (member major-mode '(c-mode c++-mode))))))
(semantic-mode 1)


;;--------------------------------------------------------------------------;;
;; define a function which adds semantic as a suggestion backend for  auto  ;;
;; completion ac hook that function to the c-mode-common-hook               ;;
;;--------------------------------------------------------------------------;;
(defun my:add-semantic-to-autocomplate()
  "Sematic mode hook."
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplate)


;; location for the semantic db
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" cache-dir))

; (global-semanticdb-minor-mode 1)

(when (featurep 'semanticdb)
  (setq semanticdb-default-save-directory
        (concat (getenv "HOME") "/.emacs.d/cache/semanticdb"))
  (when (not (file-exists-p semanticdb-default-save-directory))
    (make-directory semanticdb-default-save-directory)))

;; idle scheduler with automatically reparse buffers in idle time
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-highlight-func-mode t)
;; use company auto completion
(global-semantic-idle-completions-mode nil)
(global-semantic-decoration-mode t)
(global-semantic-show-unmatched-syntax-mode t)


;; Try to make completions when not typing
'(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
'(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;;;
;; By default, Semantic automatically includes some default system include
;; paths such as /usr/include, /usr/local/include. Specify additional ones
;;;
(semantic-add-system-include "/usr/include" 'c++-mode)
(semantic-add-system-include "/usr/local/include" 'c++-mode)
(semantic-add-system-include "/usr/local/opt/opencv3/include" 'c++-mode)
(semantic-add-system-include "/usr/local/opt/opencv3/include/opencv" 'c++-mode)
(semantic-add-system-include "/usr/local/opt/opencv3/include/opencv2" 'c++-mode)

;;;
;; Prohibit semantic from searching through system headers. We want
;; company-clang to do that for us.
;;;
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(local project unloaded recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(local project unloaded recursive))

(semantic-remove-system-include "/usr/include/" 'c++-mode)
(semantic-remove-system-include "/usr/local/include/" 'c++-mode)
(add-hook 'semantic-init-hooks
          'semantic-reset-system-include)

;;;
;; Disable tag boundary decoration:
;;;
(add-hook 'semantic-mode-hook
          '(lambda()
             (semantic-toggle-decoration-style
              "semantic-tag-boundary" -1)))


;;
;; Use info extracted from Irony to help Semantic
;;
(defadvice irony-cdb--update-compile-options (after my/irony-cdb--update-compile-options activate)
  "Pass the include paths found by Irony to Semantic."
  (dolist (dir (irony--extract-user-search-paths
                irony--compile-options
                irony--working-directory))
    (semantic-add-system-include dir)))

;;
;; force reloading all the includes after Irony update
;;
(add-to-list 'semantic-init-db-hook
             (lambda ()
               (semanticdb-find-translate-path nil nil)))


;;
;; semantic parsing
;;
(defun semantic-parse-dir (root)
  "Make Semantic parse all the source files in ROOT directory, recursively."
  (interactive
   (list (read-directory-name "Root directory; "
                             irony--working-directory)))
  (dolist (file (directory-files-recursively
                 root
                 ".*\\.\\(c\\|cpp\\|cxx\\|h\\|hpp\\|hxx\\)"))
    (semanticdb-file-table-object file)))

;;===========================================================================

(provide 'semantic-config)
;;; semantic-config.el ends here
