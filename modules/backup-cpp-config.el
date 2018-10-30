;;; package --- c/c++ configuration settings cpp-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-config.el
;;; Description: C/C++ IDE Support
;;;
;;; run the below and add into ..clang_complete
;;; echo | clang -x c++ -v -E - 2>&1
;;; | sed -n '/^#include </,/^End/s|^[^/]*\([^ ]*/include[^ ]*\).*$|-I\1|p'
;;;
;;; cpp -v
;;;
;;; elisp code for customizing the C/C++ development on Emacs
;;; reference http://nilsdeppe.com/posts/emacs-c++-ide
;;; https://github.com/google/styleguide
;;;
;;; a sample c++ project creation command
;;; { mkdir -p hello/src; \
;;;       printf "#include <stdio.h>\nint main(void) {\nprintf(\"hello world\"); \
;;;       \nreturn 0;\n}" > \
;;;       hello/src/main.cpp;printf "cmake_minimum_required(VERSION 2.6) \
;;;       \nadd_executable(main main.cpp)" > \
;;;       hello/src/CMakeLists.txt
;;; }
;;;
;;;
;;; Code:
;;;
;;;;===========================================================================
(require 'cl)
(require 'cc-mode)                      ;; major mode for c and similar languages
(require 'company-c-headers)            ;; company backends for completing C/C++ headers
(require 'c-eldoc)                      ;; helpful c documentation
(require 'google-c-style)               ;; google's c/c++ style for c-mode
(require 'clang-format)                 ;; code formatting

;;------------------------------------------------------------------------------
;; OSX System base path for Xcode platform
;;------------------------------------------------------------------------------
(defvar osx-base-path "/Applications/Xcode.app/Contents/Developer/Platforms/")

;; Easy realtime C++ syntax check and IntelliSense with CMake.
(require-package 'cpputils-cmake)

;; helper utilities configured separately
(require 'cpp-helper-config)


;;------------------------------------------------------------------------------
;; code indentation (making code gnu style)
;;------------------------------------------------------------------------------
(defun c-wx-lineup-topmost-intro-cont (langelem)
  "LANGELEM WxWidgets handling."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
      'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

;; set a style for the code - avoid default "gnu" style
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

  ;; make a #define to be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))

  (when buffer-file-name
    ;; @see https://github.com/redguardtoo/cpputils-cmake
    ;; Make sure your project uses cmake!
    ;; Or else, you need to comment out below code to prevent warnings
    ;; flymake buffer check
    (after 'flymake
      (if (executable-find "cmake")
          (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                       (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
              (cppcm-reload-all))))))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  "Mode specific settings."
  (unless (is-buffer-file-temp)
    (my-common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode) (derived-mode-p 'groovy-mode))
      (my-c-mode-setup))

    ;; gtags (GNU global) stuff
    (when (and (executable-find "global")
               ;; `man global' to figure out why
               (not (string-match-p "GTAGS not found"
                                    (shell-command-to-string "global -p"))))
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
(global-set-key [C-M-tab] 'clang-format-region)


;;------------------------------------------------------------------------------
;; Completion settings for Auto-Complete
;; ac-clang-flags to include from echo "" | g++ -v -x c++ -E -
;;------------------------------------------------------------------------------
(after "auto-complete"

  ;; define a function which initializes auto-complete-c-headers and then
  ;; gets called for the relevant c/c++ hooks
  (defun my:ac-c-header-init ()
    "Auto completion using ac."
    (require-package 'auto-complete-c-headers)
    (require 'auto-complete-c-headers)      ;; auto-complete source for C/C++ header files
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
    ;; (add-to-list 'ac-sources 'ac-source-c-headers)
    ;; execute command `gcc -xc++ -E -v -` to find the header directories
    (add-to-list 'achead:include-directories '"/usr/include")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
    (add-to-list 'achead:include-directories '"/System/Library/Frameworks")
    (add-to-list 'achead:include-directories '"/Library/Frameworks")
    ;; below two additional ones apart from gcc output
    (add-to-list 'achead:include-directories '"/usr/local/opt/gcc/include/c++/8.1.0")
    (add-to-list 'achead:include-directories '"/usr/include/c++/4.2.1")
    )

  ;; now let's call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:ac-c-header-init)
  (add-hook 'c-mode-hook 'my:ac-c-header-init)

  (defun aqua/auto-complete-clang-setup ()
    "auto-complete mode add the necessary include locations."
    ;; auto complete source for clang. AC+Clang+Yasnippet
    (require-package 'auto-complete-clang)
    (require 'auto-complete-clang)
    (local-set-key (kbd "C-S-<return>") 'ac-complete-clang)
    (setq command "echo | g++ -v -x c++ -E - 2>&1 |
                   grep -A 20 starts | grep include | grep -v search")
    (setq ac-clang-flags
          (mapcar (lambda (item)
                    (concat "-I" item))
                  (split-string
                   (shell-command-to-string command))))
    ;; completion for C/C++ macros.
    (push "-code-completion-macros" ac-clang-flags)
    (push "-code-completion-patterns" ac-clang-flags)
    (dolist (mode-hook '(c-mode-hook c++-mode-hook))
      (add-hook mode-hook
                (lambda ()
                  (add-to-list 'ac-sources 'ac-source-clang)))))

  (aqua/auto-complete-clang-setup)

  )


;;------------------------------------------------------------------------------
;; for auto completiion using the company mode
;; c++ header completion for standard libraries
;;------------------------------------------------------------------------------
(after "company"

  (defun my:company-c-headers-init()
    "Build the c headers for adding."
    ;;(setq company-idle-delay nil)
    (add-to-list 'company-backends 'company-c-headers)
    (mapcar (lambda (item)
              (add-to-list
               'company-c-headers-path-system item))
            '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include"
              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
              "/usr/local/include"
              "/usr/include"
              )))

  ;; delete company-semantic because it has higher precedence than company-clang
  (setq company-backends (delete 'company-semantic company-backends))
  ;; now let's call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:company-c-headers-init)
  (add-hook 'c-mode-hook 'my:company-c-headers-init)

  ;; company-clang system paths
  (setq company-clang-executable
        ;;"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++"
        (executable-find "clang"))

  ;; (setq company-clang-arguments
  ;;       '("-std=c++11"
  ;;         "-stdlib=libc++"
  ;;         "-I/System/Library/Frameworks/Python.framework/Headers"
  ;;         "-isysroot"
  ;;         "-I/usr/include/c++/4.2.1"))


  )


;;------------------------------------------------------------------------------
;; Syntax Checking with FlyMake and FlyCheck
;; flycheck and google-c-style mode
;;------------------------------------------------------------------------------
;; Force flycheck to always use c++11 support. We use
;; the clang language backend so this is set to clang
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++11")))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(custom-set-variables
 '(flycheck-c/c++-googlelint-executable (concat user-emacs-directory "/private/cpplint.py"))
 '(flycheck-google-cpplint-verbose "3")
 '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
 '(flycheck-google-cpplint-linelength "120"))

(after 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     ;; (flycheck-add-next-checker 'c/c++-clang
     ;;                       '(warning . c/c++-googlelint))
     (add-to-list 'flycheck-checkers 'c/c++-googlelint)
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint))))


;; flycheck syntax checking
;; clang include paths for flycheck
;; (setq flycheck-clang-include-path
;;   (append (mapcar
;;             (lambda (item)
;;               (concat "-I" item))
;;             (split-string
;;               "
;;                 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
;;                 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include
;;                 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
;;                 ;;/usr/local/opt/opencv3/include
;;                 /usr/local/include
;;                 /usr/include
;;                 "))
;;     flycheck-clang-include-path))

(defvar include-path
    (mapcar
        (lambda (item)
          (concat "-I" item))
        (split-string
          "
            /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
            /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include
            /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
            /usr/local/include
            /usr/include
            ")))

(after 'flycheck
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++11")
              ;; (setq flycheck-clang-include-path (list "/usr/local/include" "/usr/include"))
              (setq flycheck-clang-include-path include-path)
              (setq flycheck-c/c++-gcc-executable (executable-find "clang++"))
              )))

;; Interactively configure FlyCheck using pkg-config
;; Usage M-x flycheck-pkg-config
(require-package 'flycheck-pkg-config)

;; FlyMake checking and integration with CMake
(after "flymake"
  ;; get the required libraries
  (require 'cmake-project)
  ;;(require 'flymake-cursor)

  (defun my:flymake-google-init()
    (require 'flymake-google-cpplint)
    (custom-set-variables
     '(flymake-google-cpplint-command (executable-find "cpplint")))
    (flymake-google-cpplint-load))
  (add-hook 'c-mode-hook 'my:flymake-google-init)
  (add-hook 'c++-mode-hook 'my:flymake-google-init)

  (setq flymake-gui-warnings-enabled nil)
  (set-variable 'flymake-start-syntax-check-on-newline nil)

  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

  (defun turn-on-flymake-mode()
    (if (and (boundp 'flymake-mode) flymake-mode)
        ()
      (flymake-mode t)))

  (add-hook 'c-mode-common-hook (lambda () (turn-on-flymake-mode)))
  (add-hook 'c++-mode-hook (lambda () (turn-on-flymake-mode)))

  (defun cmake-project-current-build-command ()
    "Command line to compile current project as configured in the
  build directory."
    (concat "cmake --build "
            (shell-quote-argument (expand-file-name
                                   cmake-project-build-directory)) " -- -j 8" ))

  (defun cmake-project-flymake-init ()
    "Check for the Cmake build directory."
    (list (executable-find "cmake")
          (list "--build" (expand-file-name cmake-project-build-directory) "--" "-j" "8" )))
  )


;;-----------------------------------------------------------------------------
;; [ modern-cpp-font-lock ] -- font-locking for C++ mode
;;-----------------------------------------------------------------------------
(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :init
  ;; (modern-c++-font-lock-global-mode t)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


;;-----------------------------------------------------------------------------
;; [ auto-complete-clang-async ] -- C/C++ auto-completion mechanism
;;-----------------------------------------------------------------------------
; (require 'auto-complete-clang-async)
; (add-hook 'c++-mode-hook
;           '(lambda()
;              (setq ac-clang-complete-executable "~/.emacs.d/bin/clang-complete")
;              (setq ac-sources '(ac-source-clang-async))
;              (ac-clang-launch-completion-process)))

; (defun ac-cc-mode-setup ()
;   (setq ac-clang-complete-executable (expand-file-name "~/.emacs.d/bin/clang-complete"))
;   (setq ac-sources (append '(ac-source-clang-async) ac-sources))
;   (setq ac-clang-cflags (mapcar (lambda (item)
;                                   (concat "-I" (expand-file-name item)))
;                                   (split-string "/opt/software/clang+llvm-6.0.0-x86_64-apple-darwin/lib/clang/6.0.0/include
; /opt/software/clang+llvm-6.0.0-x86_64-apple-darwin/include")))
;   (setq ac-clang-cflags (append '("-std=c++1y") ac-clang-cflags))
;   (ac-clang-launch-completion-process))
; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
; (add-hook 'c++-mode-common-hook 'ac-cc-mode-setup)
; (add-hook 'auto-complete-mode-hook 'ac-common-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'cpp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-config.el ends here
