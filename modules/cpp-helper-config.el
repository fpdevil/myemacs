;;; package --- helper configuration settings for C++
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-helper-config.el
;;; Description: C++ Auto Insert statements
;;;
;;; elisp code for cpp helpers
;;===========================================================================

;;;
;;; Code:
;;;

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.cpp\\|.cc\\'" . "C++ skeleton")
     '(
       "Short description:"
       "/*\n"
       " *********************************************************************************\n"
       " *    Filename      : " (file-name-nondirectory buffer-file-name) "\n"
       " *\n"
       " *    Description   : " _ "\n"
       " *\n"
       " *    Version       : 1.0\n"
       " *    Created       : " (format-time-string "%a %b %d %H:%M:%S %Z %Y") "\n"
       " *    Revision      : none\n"
       " *    Compiler      : GCC\n"
       " *\n"
       " *    Author        : " (progn user-full-name) " <" (progn user-mail-address) ">\n"
       " *    Organization  : \n"
       " *    Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
       " *********************************************************************************\n"
       " */\n\n"
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
