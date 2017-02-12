((c-mode . ((indent-tabs-mode . nil)))
 (c++-mode . ((indent-tabs-mode . nil)))
 (nil . ((flycheck-clang-language-standard . "c++11")
         (flycheck-clang-standard-library . "libc++")
         (flycheck-clang-include-path . ("."
                                         "src"
                                         "include1"
                                         "include2"))
         (company-clang-arguments . ("-I/Users/sampathsingamsetty/sw/programming/cpp/my-project/include/"
                                     "-I/Users/sampathsingamsetty/sw/programming/cpp/my-project/include1/"
                                     "-I/Users/sampathsingamsetty/sw/programming/cpp/my-project/include2/"))
         (whitespace-style . (face tabs tab-mark trailing lines-tail empty))
         (c-file-style . "stroustrup")
         (eval . (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))
         (eval . (if (boundp 'c-offsets-alist)
                     (add-to-list 'c-offsets-alist '(innamespace . -))))
)))
