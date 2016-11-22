# .emacs settings for Aquamacs

My personal emacs configuration for Aquamacs on Mac Sierra.

#### installation

Clone the repository

`git clone https://github.com/singamsetty/myemacs.git .emacs.d`

- Move the file `dotemacs` to the home folder and rename it as `.emacs`

- Main languages supported are `python3` through `jedi` and `haskell` through the `haskell-mode`
Additional customization options available through `helm`


#### Customized settings

The below section loads all the customized cnfiguration settings. If we want to add additional configuration for a new language or a package, simply create `language=config.el` in `modules/` and it will automatically be loaded.

```cl
(loop for name in configs
      do (load (concat (file-name-directory load-file-name)
                       "modules/"
                       name ".el")))
```

#### license

MIT
