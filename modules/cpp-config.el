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
;;; Code:
;;;
;;;;===========================================================================
(require 'cl)
(require 'cc-mode)                      ;; major mode for c and similar languages
(require 'company-c-headers)            ;; company backends for completing C/C++ headers
(require 'c-eldoc)                      ;; helpful c documentation
(require 'auto-complete-clang)          ;; auto complete source for clang. AC+Clang+Yasnippet
(require 'google-c-style)               ;; google's c/c++ style for c-mode
(require 'clang-format)                 ;; code formatting

(require-package 'cpputils-cmake)

; (setq cpp-helper-config
;       (expand-file-name "cpp-helper-config.el" module-dir))
; (when (file-exists-p cpp-helper-config)
;   (load cpp-helper-config 'noerror))
;; (load-file (concat module-dir "/cpp-helper-config.el"))
(require 'cpp-helper-config)

;;------------------------------------------------------------------------------
;;; OSX System base path for Xcode platform
;;------------------------------------------------------------------------------
(defvar osx-base-path "/Applications/Xcode.app/Contents/Developer/Platforms/")

;;------------------------------------------------------------------------------
;;; code indentation (making code gnu style)
;;------------------------------------------------------------------------------
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "KEY VAL Remove the old element."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new values
  (add-to-list 'c-offsets-alist '(key . val)))

(defun my-common-cc-mode-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4)
  ;; do not insert newline automatically after electric expressions
  (setq c-auto-newline nil)
  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
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

(defun c-wx-lineup-topmost-intro-cont (langelem)
  "WSWidgets handling."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
      'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

(defun my-c-mode-setup ()
  "C/C++ only setup."
  (message "my-c-mode-setup called (buffer-file-name)=%s" (buffer-file-name))
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include"
                                "$WXWIN/include"))

  ;; wxWidgets setup
  (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft)))
  ;; flymake buffer check
  (when buffer-file-name
    ;; @see https://github.com/redguardtoo/cpputils-cmake
    ;; Make sure your project use cmake!
    ;; Or else, you need comment out below code:
    (after 'flymake
    (if (executable-find "cmake")
        (if (not (or (string-match "^/usr/local/include/.*" buffer-file-name)
                     (string-match "^/usr/src/linux/include/.*" buffer-file-name)))
            (cppcm-reload-all))))))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
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
  (define-key c-mode-base-map (kbd "RET") 'c-context-line-break))

(add-hook 'c-initialization-hook 'aqua/c-initialization-hook)

;;------------------------------------------------------------------------------
;; makefile settings
;;------------------------------------------------------------------------------
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;;------------------------------------------------------------------------------
;;; Format c/c++ code using clang-format /usr/local/bin/clang-format
;;        we may create a local clang-format file using google style
;;        clang-format -style=google -dump-config > .clang-format
;;------------------------------------------------------------------------------
(global-set-key [C-M-tab] 'clang-format-region)

;;------------------------------------------------------------------------------
;;; flycheck and google-c-style mode
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


;;------------------------------------------------------------------------------
;;; auto-complete settings
;;------------------------------------------------------------------------------

;;; auto-complete mode add necessary include locations
;;; ac-clang-flags to include from echo "" | g++ -v -x c++ -E -
(defun aqua/auto-complete-clang-setup ()
  (require 'auto-complete-clang)
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

;; (setq ac-clang-flags (append
;;       (mapcar (lambda (item)(concat "-I" item))
;;               (split-string
;;                "
;; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
;; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include
;; /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
;; /usr/local/opt/opencv3/include
;; /usr/local/include
;; /usr/include
;; ")) ac-clang-flags))


;;; define a function which initializes auto-complete-c-headers and then
;;; gets called for the relevant c/c++ hooks
(defun my:ac-c-header-init ()
  "Auto completion using ac."
  (require 'auto-complete-c-headers)      ;; auto-complete source for C/C++ header files
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
  ;; execute command `gcc -xc++ -E -v -` to find the header directories
  (add-to-list 'achead:include-directories '"/usr/include")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
  (add-to-list 'achead:include-directories '"/System/Library/Frameworks")
  (add-to-list 'achead:include-directories '"/Library/Frameworks")
  ;; below two additional ones apart from gcc output
  (add-to-list 'achead:include-directories '"/usr/local/opt/gcc/include/c++/7.2.0")
  (add-to-list 'achead:include-directories '"/usr/include/c++/4.2.1")
  )

;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;------------------------------------------------------------------------------
;;; set LD_LIBRARY_PATH (for linux) / DYLD_LIBRARY_PATH (for mac)
;;------------------------------------------------------------------------------
;(setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/")
(setenv "LC_CTYPE" "UTF-8")
(setenv "DYLD_LIBRARY_PATH"
  (concat "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/"
          ":"
          "/usr/local/opt/opencv3/lib/"))

;;------------------------------------------------------------------------------
;; for auto completiion using the company mode
;; c++ header completion for standard libraries
;;------------------------------------------------------------------------------
;; (add-to-list 'company-c-headers-path-system "/usr/local/opt/opencv3/include")
(defun my:company-c-headers-init()
  "Build the c headers for adding."
  ;;(setq company-idle-delay nil)
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (mapcar (lambda (item)
            (add-to-list
             'company-c-headers-path-system item))
          '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
            "/usr/local/include"
            "/usr/include"
           )))

(after "company"
  ;; delete company-semantic because it has higher precedence than company-clang
  (setq company-backends (delete 'company-semantic company-backends))
  ;; now let's call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:company-c-headers-init)
  (add-hook 'c-mode-hook 'my:company-c-headers-init))

;; company-clang system paths
(setq company-clang-executable
      ;;"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++"
      (executable-find "clang"))

; (setq company-clang-arguments
;       '("-std=c++11"
;         "-stdlib=libc++"
;         "-I/System/Library/Frameworks/Python.framework/Headers"
;         "-isysroot"
;         "-I/usr/include/c++/4.2.1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cmake-project)
(require 'flymake-cursor)

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
  (list (executable-find "cmake")
        (list "--build" (expand-file-name cmake-project-build-directory) "--" "-j" "8" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck syntax checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clang include paths for flycheck
; (setq flycheck-clang-include-path
;   (append (mapcar
;             (lambda (item)
;               (concat "-I" item))
;             (split-string
;               "
;                 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
;                 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include
;                 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
;                 ;;/usr/local/opt/opencv3/include
;                 /usr/local/include
;                 /usr/include
;                 "))
;     flycheck-clang-include-path))

(defvar include-path
    (mapcar
        (lambda (item)
          (concat "-I" item))
        (split-string
          "
            /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
            /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/9.0.0/include
            /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
            ;;/usr/local/opt/opencv3/include
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

;;-----------------------------------------------------------------------------
;;; [ modern-cpp-font-lock ] -- font-locking for C++ mode
;;-----------------------------------------------------------------------------
(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :init
  (modern-c++-font-lock-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'cpp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-config.el ends here
