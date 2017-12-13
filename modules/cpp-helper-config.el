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
;;===========================================================================

;; C++ completion for GNU Emacs
(require 'function-args)
(fa-config-default)


;; compilation database code
(defun cpp-complete/find-clang-complete-file ()
  "Find the .clang_complete file."
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

(defun cpp-complete/load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, compilation database one line at a time"
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ;; replace the relative paths with absolute paths
      (while (re-search-forward cc-file "\\(-I\\|-isystem\n\\)\\(\\S-+\\)" nil t)
        (replace-match (format "%s%s" (match-string 1)
                               (expand-file-name (match-string 2) invocation-dir))))
      ;; turn the lines to a list
      (setq compile-flags
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

;; Now load the compilation database
(defun cpp-complete/load-clang-args ()
  "Set arguments for company-clang, system paths for company-c-headers and
arguments for flycheck-clang based on project specific file"
  (unless company-clang-arguments
    (let* ((cc-file (company-mode/find-clang-complete-file))
           (flags (if cc-file (company-mode/load-clang-complete-file cc-file) '()))
           (dirs (mapcar (lambda (f) (substring f 2))
                         (remove-if-not (lambda (f) (string-prefix-p "-I" f)) flags))))
      (setq-local company-clang-arguments flags)
      (setq-local company-c-headers-path-system (append '("/usr/include" "/usr/local/include") dirs))
      (setq-local flycheck-clang-args flags))))

(add-to-hooks 'cpp-complete/load-clang-args '(c-mode-hook c++-mode-hook))

;; eldoc support
(with-eval-after-load 'c-eldoc
 (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
 (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
 (setq c-eldoc-cpp-command (executable-find "clang"))
 )

;; auto-insert the comment strings at top based on the file types
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


;;---------------------------------------------------------------------------
;; compiling c and cpp files
;;---------------------------------------------------------------------------
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cpp-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-helper-config.el ends here
