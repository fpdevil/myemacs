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
       "/*"
       "\n * " (file-name-nondirectory (buffer-file-name))
       "\n */" > \n \n
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
       "int main (int argc, char *argv[])\n"
       "{" \n
       > _ \n
       "return 0;\n"
       "}" > \n
       )))

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.h\\|.hpp\\'" . "c/c++ header")
     '((s-upcase (s-snake-case (file-name-nondirectory buffer-file-name)))
       "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif  // " str)))

(provide 'cpp-helper-config)

;;; cpp-helper-config.el ends here
