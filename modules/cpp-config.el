;;; package --- c/c++ configuration settings cpp-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-config.el
;;; Description: C/C++ IDE Support using the company and irony modes
;;;
;;; elisp code for customizing the C/C++ development on Emacs
;;; reference http://nilsdeppe.com/posts/emacs-c++-ide
;;===========================================================================
(require 'cl)
(require 'irony)                        ;; irony cpp ide plugin
(require 'company-c-headers)            ;; company backends for completing C/C++ headers
(require 'company-irony-c-headers)      ;; company backend for irony c-headers
(require 'irony-eldoc)                  ;; eldpc support for irony
(require 'flycheck-irony)               ;; flycheck checker for the C, C++ and Objective-C languages
(require 'auto-complete-clang)          ;; auto complete source for clang. AC+Clang+Yasnippet
(require 'auto-complete-c-headers)      ;; auto-complete source for C/C++ header files
(require 'google-c-style)               ;; google's c/c++ style for c-mode
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup irony modes for c/c++                                              ;;
;; https://github.com/Sarcasm/irony-mode/wiki/Mac-OS-X-issues-and-workaround;;
;; refer my own instalaltion log in root irony-install.md                   ;;
;; ---------------------------- check commands ---------------------------- ;;
;; xcodebuild -find make                                                    ;;
;; xcodebuild -find gcc                                                     ;;
;; xcodebuild -find g++                                                     ;;
;; xcodebuild -find clang                                                   ;;
;; xcodebuild -find clang++                                                 ;;
;;                                                                          ;;
;; echo echo "" | g++ -v -x c++ -E -                                        ;;
;; echo echo "" | gcc -xc -E -v -                                           ;;
;; echo echo "" | gcc -xc++ -E -v -                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook '(c++-mode-hook
                c-mode-hook
                objc-mode-hook))
  (add-hook hook 'irony-mode))

;; irony cusomized locations for binary, prefix and source
(setq irony-cmake-executable "/usr/local/bin/cmake")
(setq irony-server-install-prefix (concat (getenv "HOME") "/.emacs.d/irony/"))
(setq irony-user-dir (concat (getenv "HOME") "/.emacs.d/irony/"))


;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; add hooks to irony-mode
(dolist (mode '(my-irony-mode-hook
                irony-cdb-autosetup-compile-options
                irony-eldoc))
  (add-hook 'irony-mode-hook mode))


;; Load with the `irony-mode` as a grouped back-end
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony company-c-headers)))

;; company-irony setup, c-header completions
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; using flycheck for irony mode                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-clang-flags to include from echo "" | g++ -v -x c++ -E -            ;;;
;;; add necessary include locations                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-clang-flags
      (mapcar (lambda (item) (concat "-I" item))
              (split-string
               "
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
/usr/local/Cellar/opencv3/HEAD-7dd3723_4/include
/usr/local/include
/usr/include
")
               ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete settings                                                 ;;;
;;; define a function which initializes auto-compelte-c-headers and then   ;;;
;;; gets called for the relevant c/c++ hooks                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:ac-c-header-init ()
  "Auto completion using ac."
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; execute command `gcc -xc++ -E -v -` to find the header driectories
  (add-to-list 'achead:include-directories '"/usr/local/opt/gcc/include/c++/6.3.0")
  (add-to-list 'achead:include-directories '"/usr/include/c++/4.2.1")
  (add-to-list 'achead:include-directories '"/usr/local/Cellar/opencv3/HEAD-7dd3723_4/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  (add-to-list 'achead:include-directories '"/usr/include")
  )

; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set LD_LIBRARY_PATH / DYLD_LIBRARY_PATH (for mac)                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/")
(setenv "DYLD_LIBRARY_PATH"
  (concat "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/"
          ":"
          "/usr/local/Cellar/opencv3/HEAD-7dd3723_4/lib/"))


(defun my:ac-cc-mode-setup ()
  "AutoComplete CC Mode."
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my:ac-cc-mode-setup)


;;--------------------------------------------------------------------------;;
;; for company completion                                                   ;;
;; c++ header completion for standard libraries                             ;;
;;--------------------------------------------------------------------------;;
(add-to-list 'company-c-headers-path-system "/usr/local/Cellar/opencv3/HEAD-7dd3723_4/include")
(defun my:company-c-headers-init()
  ;;(setq company-idle-delay nil)
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (mapcar (lambda (item) (add-to-list 'company-c-headers-path-system item))
          '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
            "/usr/local/opt/gcc/include/c++/6.3.0"
            "/usr/include/c++/4.2.1"
            "/usr/local/Cellar/opencv3/HEAD-7dd3723_4/include"
            "/usr/local/include"
            "/usr/include"
           )
          )
  )
;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:company-c-headers-init)
(add-hook 'c-mode-hook 'my:company-c-headers-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; turn on Semantic                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)
(require 'semantic)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; By default, Semantic automatically includes some default system include
;; paths such as /usr/include, /usr/local/include. Specify additional ones
(semantic-add-system-include "/usr/local/Cellar/opencv3/HEAD-7dd3723_4/include" 'c++-mode)

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

; turn on ede mode
(global-ede-mode 1)

;; project
; (ede-cpp-root-project "my project" :file "~/demos/my_program/src/main.cpp"
;           :include-path '("/../my_inc"))

;; location for the semanticdb
(setq semanticdb-default-save-directory (concat (getenv "HOME") "/.emacs.d/cache/semanticdb"))
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-highlight-func-mode t)
(global-semantic-idle-completions-mode nil) ;; use company/ac
(global-semantic-decoration-mode t)
(global-semantic-show-unmatched-syntax-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load))
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google-c-style mode                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


(provide 'cpp-config)

;;; cpp-config.el ends here