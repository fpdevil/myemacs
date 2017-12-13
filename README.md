# .emacs settings for Aquamacs

My personal emacs configuration for Aquamacs on Mac Sierra.

#### installation

Clone the repository

`git clone https://github.com/fpdevil/myemacs.git .emacs.d`

- Move the file `dotemacs` to the home folder and rename it as `.emacs`

- Main languages supported are `python3` through `jedi` and `haskell` through the `haskell-mode`
Additional customization options available through `helm`


#### Customized settings

For package installation, the following are the main required libraries

```elisp
(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'package)
```

The below section loads all the customized cnfiguration settings. If we want to add additional configuration for a new language or a package, simply create `mode-config.el` in `modules/` and it will automatically be loaded.

```elisp
(cl-loop for name in configs
         do (load (concat (file-name-directory load-file-name)
                          "modules/"
                          name ".el")))
```

For the packages which are not yet available through melpa or any repository, we can download the same to a directoy and load them manually. For the current setup, a directory vendor will be used to host such packages and the following section will be used to load the packages

```elisp
(cl-loop for location in custom-load-paths
         do (add-to-list 'load-path
                         (concat (file-name-directory (or load-file-name
                                                          (buffer-file-name)))
                                 "vendor/"
                                 location)))
```


#### For creating custom per project definitions and settings use a
`.dir-locals.el` file as defined here.

Open the root directory of your project in Dired with `C-x d`, and
then type `M-x` *add-dir-local-variable* RET `c++-mode` RET
`flycheck-gcc-language-standard` RET `c++11`. This will create a
`.dir-locals.el` file in the root directory of your project. Emacs
reads this file whenever you visit a file from this directory or any
subdirectory, and sets variables according to the rules in this
file. Specifically, Emacs will now set the language standard for
Flycheck syntax checking to C++ 11 for all C++ files in your
project. Here is the content of the just created `dir-locals.el` file.

``` emacs-lisp
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((c++-mode
  (flycheck-gcc-language-standard . c++11)))
```

#### References
* [Elisp Programming] - A wonderful overview of elisp!
* [Functional Programming] - Functional Programming by Example!

[Elisp Programming]: <http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html>
[Functional Programming]: <http://caiorss.github.io/Functional-Programming/>

#### license

MIT
