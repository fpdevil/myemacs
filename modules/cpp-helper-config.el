;;; package --- helper configuration settings for C++
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-helper-config.el
;;; Description: C++ Auto Insert statements
;;;
;;; elisp code for cpp helpers
;;;
;;; Code:
;;;
;;;;===========================================================================
(require 'cl)
(require 'cc-mode)           ;; major mode for c and similar languages
(require 'google-c-style)    ;; google's c/c++ style for c-mode
(require 'c-eldoc)           ;; helpful c documentation

;; file support
;; This adds additional extensions which indicate files normally
;; handled by cc-mode.
(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'"   . c-mode))


;;------------------------------------------------------------------------------
;; OSX System base path for Xcode platform
;;------------------------------------------------------------------------------
(defvar osx-base-path "/Applications/Xcode.app/Contents/Developer/Platforms/")

;;------------------------------------------------------------------------------
;; set LD_LIBRARY_PATH (for linux) / DYLD_LIBRARY_PATH (for mac)
;;------------------------------------------------------------------------------
;;(setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/")
(setenv "LC_CTYPE" "UTF-8")
(setenv "DYLD_LIBRARY_PATH" "/opt/software/clang+llvm-7.0.0-x86_64-apple-darwin/lib/")

;;------------------------------------------------------------------------------
;; code indentation (making code gnu style)
;;------------------------------------------------------------------------------
(defun c-wx-lineup-topmost-intro-cont (langelem)
  "LANGELEM WxWidgets handling."
  (save-excursion (beginning-of-line)
                  (if (re-search-forward "EVT_" (line-end-position) t) 'c-basic-offset
                    (c-lineup-topmost-intro-cont langelem))))

;; set a default style for the code - avoid default "gnu" style
(setq c-default-style "linux")

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "KEY VAL Remove the old element."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new values
  (add-to-list 'c-offsets-alist '(key . val)))

(defun my-common-cc-mode-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)."
  (setq c-basic-offset 4)
  ;; do not insert newline automatically after electric expressions
  (setq c-auto-newline nil)
  ;; run compile command without hitting Enter
  (setq compilation-read-command nil)
  ;; syntax-highlight aggressively
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)
  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  (c-toggle-electric-state -1)
  ;;
  ;; code indentation
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))


(defun my-c-mode-setup ()
  "C/C++ only specific setup."
  (message "my-c-mode-setup called (buffer-file-name)=%s" (buffer-file-name))
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-n") 'ff-find-other-file)
  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include"
                                "$WXWIN/include"))


  ;; wxWidgets setup
  (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)
  (c-set-offset 'substatement-open 0)   ; curly braces alignment
  (c-set-offset 'case-label 4)          ; switch case statements alignment
  (add-to-list 'imenu-generic-expression '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1))

  ;; make a #define to be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))
  (when buffer-file-name
    ;; @see https://github.com/redguardtoo/cpputils-cmake
    ;; Make sure your project uses cmake!
    ;; Or else, you need to comment out below code to prevent warnings
    ;; flymake buffer check
    (after 'flymake (if (executable-find "cmake")
                        (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                                     (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
                            (cppcm-reload-all))))))


;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  "Mode specific settings."
  (unless (aqua/is-buffer-file-temp)
    (my-common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode)
                (derived-mode-p 'groovy-mode))
      (my-c-mode-setup))

    ;; gtags (GNU global) stuff
    (when (and (executable-find "global")
               ;; `man global' to figure out why
               (not (string-match-p "GTAGS not found" (shell-command-to-string "global -p"))))
      ;; emacs 24.4+ will set up eldoc automatically.
      ;; so below code is NOT needed.
      (eldoc-mode 1))))

(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)

;;------------------------------------------------------------------------------
;; some basic cc mode settings
;;------------------------------------------------------------------------------
(defun aqua/c-initialization-hook ()
  "CC mode key and line break."
  (define-key c-mode-base-map (kbd "RET") 'c-context-line-break))
(add-hook 'c-initialization-hook 'aqua/c-initialization-hook)

;;------------------------------------------------------------------------------
;; Makefile tab handling settings
;;------------------------------------------------------------------------------
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)))

;;------------------------------------------------------------------------------
;;  Format c/c++ code using clang-format /usr/local/bin/clang-format
;;        we may create a local clang-format file using google style
;;        clang-format -style=google -dump-config > .clang-format
;;------------------------------------------------------------------------------
(use-package clang-format
  :ensure t
  :after cc-mode
  :defines c-mode-base-map
  :bind (:map c-mode-base-map ("C-S-f" . clang-format-region))
  :config
  (use-package sarcasm-clang-format
    :defer t
    :commands sarcasm-clang-format-set-c-style
    :init (add-hook 'c++-mode-hook 'sarcasm-clang-format-set-c-style)))

;; c++ completion for GNU Emacs
(require-package 'function-args)
(require 'function-args)
(fa-config-default)
(setq fa-insert-method 'name-space-parens)
(define-key function-args-mode-map (kbd "M-i") nil)
(define-key function-args-mode-map (kbd "C-2") 'fa-show)


;;------------------------------------------------------------------------------
;; [FlyCheck] - Syntax checker configuration...
;; flycheck and google-c-style mode
;;------------------------------------------------------------------------------
;; Force flycheck to always use c++11 support. We use
;; the clang language backend so this is set to clang
;;(add-hook 'c++-mode-hook
;;          (lambda ()
;;            (setq flycheck-clang-language-standard "c++14")))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(custom-set-variables '(flycheck-c/c++-googlelint-executable (concat user-emacs-directory
                                                                     "private/cpplint.py"))
                      '(flycheck-google-cpplint-verbose "3")
                      '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
                      '(flycheck-google-cpplint-linelength "120"))

(after 'flycheck '(progn
                    (require 'flycheck-google-cpplint)
                    ;; Add Google C++ Style checker.
                    ;; In default, syntax checked by Clang and Cppcheck.
                    ;; (flycheck-add-next-checker 'c/c++-clang
                    ;;                       '(warning . c/c++-googlelint))
                    (add-to-list 'flycheck-checkers 'c/c++-googlelint)
                    (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))))

(defvar include-path
  (mapcar
   (lambda (item)
     (concat "-I" item))
   (split-string "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
                  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.1.0/include
                  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
                  /usr/local/include
                  /usr/include")))

(after 'flycheck (add-hook 'c++-mode-hook
                           (lambda ()
                             (setq flycheck-clang-language-standard "c++14")
                             ;; (setq flycheck-clang-include-path (list "/usr/local/include" "/usr/include"))
                             (setq flycheck-clang-include-path include-path)
                             (setq flycheck-c/c++-gcc-executable (executable-find "clang++")))))


  ;; Integrate Clang Static Analyzer with flycheck for on-the-fly
  ;; static analysis in Emacs
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :init
  (setq flycheck-clang-analyzer-executable "/opt/software/clang+llvm-7.0.0-x86_64-apple-darwin/bin/clang")
  :config (flycheck-clang-analyzer-setup))


;; Interactively configure FlyCheck using pkg-config
;; Usage M-x flycheck-pkg-config
(use-package flycheck-pkg-config
  :ensure t
  )

;;------------------------------------------------------------------------------
;; [FlyMake] - syntax checking and integration with CMake
;;------------------------------------------------------------------------------
(after "flymake"
  ;; get the required libraries
  (require 'cmake-project)
  ;;(require 'flymake-cursor)
  (defun my:flymake-google-init()
    (require 'flymake-google-cpplint)
    (custom-set-variables '(flymake-google-cpplint-command (executable-find "cpplint")))
    (flymake-google-cpplint-load))
  (add-hook 'c-mode-hook 'my:flymake-google-init)
  (add-hook 'c++-mode-hook 'my:flymake-google-init)
  (setq flymake-gui-warnings-enabled nil)
  (set-variable 'flymake-start-syntax-check-on-newline nil)
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt")
        (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
  (defun turn-on-flymake-mode()
    (if (and (boundp 'flymake-mode)
             flymake-mode)
        ()
      (flymake-mode t)))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (turn-on-flymake-mode)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (turn-on-flymake-mode)))
  (defun cmake-project-current-build-command ()
    "Command line to compile current project as configured in the
  build directory."
    (concat "cmake --build " (shell-quote-argument (expand-file-name cmake-project-build-directory))
            " -- -j 8" ))
  (defun cmake-project-flymake-init ()
    "Check for the Cmake build directory."
    (list (executable-find "cmake")
          (list "--build" (expand-file-name cmake-project-build-directory) "--" "-j" "8" ))))


;;-----------------------------------------------------------------------------
;; [ modern-cpp-font-lock ] -- font-locking for C++ mode
;;-----------------------------------------------------------------------------
(use-package
  modern-cpp-font-lock
  :ensure t
  :defer t
  :init
  ;; (modern-c++-font-lock-global-mode t)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


;; show #if 0 / #endif etc regions in comment face - taken from
;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa
(defun c-mode-font-lock-if0 (limit)
  "Fontify #if 0 / #endif as comments for c modes etc.
Bound search to LIMIT as a buffer position to find appropriate
code sections."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;------------------------------------------------------------------------------
;; [eldoc] - documentation support
;;------------------------------------------------------------------------------
(with-eval-after-load 'c-eldoc
 (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
 (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
 (setq c-eldoc-cpp-command (executable-find "clang")))


;;------------------------------------------------------------------------------
;; auto-insert the comment strings at top based on the file types
;;------------------------------------------------------------------------------
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.cpp\\|.cc\\'" . "C++ skeleton")
     '(
       "Short description:"
       "//\n"
       "//"  (file-name-nondirectory buffer-file-name) "\n"
       "//\n"
       "//\n"
       "//  Created by " (progn user-full-name) " on " (format-time-string "%a %b %d %H:%M:%S %Z %Y") "\n"
       "//  Copyright © " (substring (current-time-string) -4) " " (user-full-name) ". All rights reserved." "\n\n"
    ;;" //  " (progn user-full-name) " <" (progn user-mail-address) ">\n"
       "// \n\n"
       "#include <iostream>" \n
       "//#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".hpp\"" \n \n
       "using namespace std;" \n \n
       "int main (int argc, char *argv[])"
       "\n{" \n
       > _ \n
       "return 0;"
       "\n}" > \n
       )))

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description:"
       "/*\n"
       " * " (file-name-nondirectory (buffer-file-name)) "\n"
       " */" > \n \n
       "#include <stdio.h>" \n
       "//#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int main (int argc, char *argv[]) {"\n
       > _ \n
       "return 0;\n"
       "}" > \n
       )))

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.h\\|.hpp\\'" . "c/c++ header")
     '((s-upcase (s-snake-case (file-name-nondirectory buffer-file-name)))
       "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif  // " str)))


;;
;; compiling c and cpp files
(defun compile-current-c ()
  "Compile the current c file."
  (interactive)
  (save-buffer)
  (if (file-exists-p (file-name-base)) (delete-file (file-name-base)))
  (shell-command (format "gcc -Wall %s -o %s"
                         (buffer-real-name)
                         (file-name-base))))

(defun compile-current-cpp ()
  "Compile the current c++ file."
  (interactive)
  (save-buffer)
  (if (file-exists-p (file-name-base)) (delete-file (file-name-base)))
  (shell-command (format "g++ -Wall -std=c++11 %s -o %s && echo '================================================' && ./%s"
                         (buffer-real-name)
                         (file-name-base)
                         (file-name-base))))


;;------------------------------------------------------------------------------
;; using gtags
;;------------------------------------------------------------------------------
(use-package ggtags
:ensure t
:config
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)))))


;;------------------------------------------------------------------------------
;;** START allow us to get english language explanations of complex C++ declarations
;;** To use, select a declaration such as "char*(*(*p)[5])(int)", and then type: "C-x cdecl"
;;------------------------------------------------------------------------------
(defun cdecl ()
  "Get explanations of functions in c++."
  (interactive)
  (if (eq major-mode 'c++-mode)
      (progn (interactive) (shell-command
                            (concat "c++decl explain \"" (buffer-substring (region-beginning)
                                                                           (region-end)) "\"")))
    (progn (interactive) (shell-command
                          (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                                       (region-end)) "\"")))))
;; END allow us to get english language explanations of complex C++ declarations

;;------------------------------------------------------------------------------
;;** compilation of c++ with Make if file is available
;;------------------------------------------------------------------------------
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (concat "g++ -g -Wall -Wextra -o " (file-name-sans-extension file) " " file))))))

;; -----------------------------------------------------------------------------
;; -- C C++ C# Mode
;; -----------------------------------------------------------------------------
(add-hook 'c-mode-common-hook
          '(lambda ()
             (set-fill-column 80)
             ;;M-q to fill /** */ Paragraph Segments
             (setq paragraph-start "^[ ]*\\(///\\|\\**\\)[ ]*\\([ ]*$\\|@\\)\\|^\f")
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cpp-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-helper-config.el ends here
