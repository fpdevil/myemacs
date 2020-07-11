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
;;;


;;------------------------------------------------------------------------------
;; for auto completiion using the company mode
;; c++ header completion for standard libraries
;;------------------------------------------------------------------------------
(when (eq dotemacs-completion-engine 'company)
  (require 'company-c-headers) ;; company backends for completing C/C++ headers

  (defun company-c-headers-setup ()
    "Set headers for company backends."
    (add-to-list 'company-backends 'company-c-headers))

  (defun company-c-headers-path-user-irony ()
    "Return the user include paths for the current buffer."
    (when irony-mode (irony--extract-user-search-paths irony--compile-options
                                                       irony--working-directory)))
  ;;(setq company-c-headers-path-user #'company-c-headers-path-user-irony)

  (defun my:company-c-headers-init ()
    "Build the c headers for adding."
    ;;(setq company-idle-delay nil)
    (setq-local company-minimum-prefix-length 1)
    ;;(setq company-c-headers-path-user #'company-c-headers-path-user-irony)
    (setq gcommand "echo | g++ -v -x c++ -E - 2>&1 |
                    grep -A 20 starts |
                    grep include |
                    grep -v search")
    (setq company-c-headers-path-system (mapcar
                                         (lambda (item)
                                           item)
                                         (split-string (shell-command-to-string gcommand))))
    (add-to-list 'company-c-headers-path-system
                 "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
    (add-to-list 'company-c-headers-path-system
                 "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/10.0.1/include")
    (add-to-list 'company-c-headers-path-system
                 "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
    ;; (mapcar (lambda (item)
    ;;           (add-to-list
    ;;            'company-c-headers-path-system item))
    ;;         '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
    ;;           "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/10.0.1/include"
    ;;           "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
    ;;           "/usr/local/include"
    ;;           "/usr/include"))
    (add-to-list 'company-backends 'company-c-headers))

  (require 'company-clang)
  (defun my:company-clang ()
    "Company clang configuration."
    ;; delete company-semantic because it has higher precedence than company-clang
    (setq company-backends (delete 'company-semantic company-backends))
    (setq company-clang-arguments '("-std=c++11" "-stdlib=libc++"))
    ;; company-clang system paths
    (setq company-clang-executable
          ;;(executable-find "clang")
          "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang")
    ;;(add-to-list 'company-backends 'company-c-headers)
    (add-to-list 'company-backends 'company-clang)
    ;;(setq-local company-backends '(company-clang company-dabbrev))
    )


  ;; now let's call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:company-c-headers-init)
  (add-hook 'c-mode-hook 'my:company-c-headers-init)
  (my:company-clang)
  )

;; (setq company-clang-arguments
;;       '("-std=c++11"
;;         "-stdlib=libc++"
;;         "-I/System/Library/Frameworks/Python.framework/Headers"
;;         "-isysroot"
;;         "-I/usr/include/c++/4.2.1"))


;;------------------------------------------------------------------------------
;; [Auto Complete Clang] - Completion settings for Auto-Complete
;; ac-clang-flags to include from echo "" | g++ -v -x c++ -E -
;;------------------------------------------------------------------------------
(when (eq dotemacs-completion-engine 'auto-complete)
  ;; define a function which initializes auto-complete-c-headers and then
  ;; gets called for the relevant c/c++ hooks
  (defun my:ac-c-header-init ()
    "Auto completion using ac."
    ;; auto-complete source for C/C++ header files
    (require 'auto-complete-c-headers)
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
    (add-to-list 'ac-sources 'ac-source-c-headers)

    ;; execute command `gcc -xc++ -E -v -` to find the header directories
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/10.0.1/include")
    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
    (add-to-list 'achead:include-directories '"/usr/local/opt/gcc/include/c++/9.1.0")
    (add-to-list 'achead:include-directories '"/usr/local/include")
    (add-to-list 'achead:include-directories '"/usr/include"))


  ;; now let's call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:ac-c-header-init)
  (add-hook 'c-mode-hook 'my:ac-c-header-init)

  (defun aqua/auto-complete-clang-setup ()
    "auto-complete mode add the necessary include locations."
    ;; auto complete source for clang. AC+Clang+Yasnippet
    (require 'auto-complete-clang)

    ;; (setq ac-auto-start t)
    ;; (setq ac-quick-help-delay 0.5)
    (setq ac-clang-executable
          "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang")
    (local-set-key (kbd "C-S-<return>") 'ac-complete-clang)
    (local-set-key (kbd "M-<Tab>") 'auto-complete)
    (setq command "echo | g++ -v -x c++ -E - 2>&1 |
                   grep -A 20 starts | grep include | grep -v search")
    (setq ac-clang-flags
          (append '("-std=c++14")
                  (mapcar
                   (lambda (item)
                     (concat "-I" item))
                   (split-string (shell-command-to-string command)))))

    ;; completion for C/C++ macros.
    (push "-code-completion-macros" ac-clang-flags)
    (push "-code-completion-patterns" ac-clang-flags)
    )

  (defun aqua/my-ac-cc-mode-setup ()
    (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
  (add-hook 'c-mode-common-hook 'aqua/my-ac-cc-mode-setup)
  (aqua/auto-complete-clang-setup)

  ;;-----------------------------------------------------------------------------
  ;; [ auto-complete-clang-async ] -- C/C++ auto-completion mechanism
  ;;-----------------------------------------------------------------------------
  (require 'auto-complete-clang-async)

  ;; auto complete source for clang. AC+Clang+Yasnippet!
  (defun ac-cc-mode-setup ()
    "Setup the ac-clang specific values."
    (setq ac-clang-complete-executable "~/.emacs.d/bin/clang-complete")
    (setq clang-completion-suppress-error 't)
    (setq ac-clang-cflags
          (mapcar (lambda (item)(concat "-I" item))
                  (split-string
                   "
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1
/usr/local/include
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/10.0.1/include
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
/usr/include
")))

    (setq ac-clang-cflags (append '("-std=c++14") ac-clang-cflags))
    (setq ac-sources '(ac-source-clang-async))
    (ac-clang-launch-completion-process))

  (defun my-ac-config ()
    "Add hooks for the modes."
    (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    ;;(global-auto-complete-mode t)
    )

  (my-ac-config)
  )


(provide 'cpp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-config.el ends here
