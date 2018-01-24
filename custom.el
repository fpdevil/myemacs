(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-customization-version-id 0 t)
 '(custom-enabled-themes (quote (badger)))
 '(custom-safe-themes
   (quote
    ("9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" default)))
 '(flycheck-c/c++-googlelint-executable (concat user-emacs-directory "/private/cpplint.py"))
 '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
 '(flycheck-google-cpplint-linelength "120")
 '(flycheck-google-cpplint-verbose "3")
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂")
 '(git-gutter:modified-sign "☁")
 '(git-gutter:window-width 2)
 '(haskell-font-lock-symbols t)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-notify-p t)
 '(haskell-process-args-ghci (quote nil))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save t)
 '(hindent-style "chris-done")
 '(package-selected-packages
   (quote
    (helm-themes zerodark-theme zenburn-theme yaml-mode xref-js2 xah-math-input x-path-walker wttrin which-key web-mode web-beautify volatile-highlights visual-regexp virtualenvwrapper vimrc-mode use-package twilight-bright-theme tern-auto-complete tango-plus-theme switch-window sunshine sublime-themes sphinx-doc spacemacs-theme spaceline solarized-theme smart-mode-line-powerline-theme shm rainbow-mode rainbow-identifiers rainbow-delimiters quick-peek python-pylint pydoc-info py-yapf py-autopep8 peep-dired paper-theme ox-reveal ox-html5slide org-tree-slide org-plus-contrib org-easy-img-insert org-download org-bullets org-ac ob-http ob-async neotree monokai-theme moe-theme modern-cpp-font-lock material-theme markdown-mode manage-minor-mode majapahit-theme livid-mode latex-pretty-symbols know-your-http-well key-chord json-navigator json-mode jsfmt js3-mode js2-refactor js2-highlight-vars js-doc jedi irony-eldoc import-js imenu-list iedit htmlize hindent highlight-symbol highlight-quoted highlight-numbers hi2 helm-swoop helm-projectile helm-fuzzier helm-flycheck helm-flx helm-describe-modes helm-descbinds helm-dash helm-company helm-clojuredocs helm-cider helm-c-yasnippet helm-ag haskell-snippets guide-key-tip gruvbox-theme graphviz-dot-mode google-c-style golint golden-ratio go-guru go-eldoc go-autocomplete git-gutter ghci-completion fuzzy function-args flyspell-lazy flymake-python-pyflakes flymake-hlint flymake-google-cpplint flymake-go flymake-cursor flycheck-ycmd flycheck-tip flycheck-rebar3 flycheck-pos-tip flycheck-plantuml flycheck-mix flycheck-irony flycheck-inline flycheck-haskell flycheck-elixir flycheck-color-mode-line flycheck-clojure flatui-theme fiplr exec-path-from-shell evil-visualstar evil-surround evil-smartparens evil-paredit evil-numbers evil-mc evil-matchit evil-magit evil-leader evil-indent-textobject evil-exchange evil-ediff evil-commentary evil-avy evil-anzu esup epresent ensime elpy elixir-yasnippets elisp-slime-nav ein edts ecb dumb-jump dracula-theme doom-themes discover-my-major dired-rainbow dired-imenu dired-filter dired-details+ dired+ diminish dim darkokai-theme cyberpunk-theme cpputils-cmake company-ycmd company-web company-try-hard company-tern company-quickhelp company-math company-jedi company-irony-c-headers company-irony company-go company-ghci company-ghc company-flx company-distel company-dict company-cabal company-c-headers color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-molokai color-theme color-identifiers-mode coffee-mode cmake-project clojure-snippets clojure-mode-extra-font-locking clippy clang-format cherry-blossom-theme c-eldoc buffer-move benchmark-init beacon badger-theme auto-complete-nxml auto-complete-distel auto-complete-clang auto-complete-c-headers airline-themes afternoon-theme ace-mc ac-slime ac-js2 ac-haskell-process ac-etags ac-emmet ac-cider ac-alchemist)))
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((((class color)) (:underline "red"))))
 '(flycheck-info ((((class color)) (:underline "green"))))
 '(flymake-errline ((((class color)) (:style wave :underline "red"))))
 '(flymake-warnline ((((class color)) (:style wave :underline "yellow"))))
 '(flyspell-duplicate ((t (:underline (:color "Blue" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "Purple" :style wave)))))
 '(header-line ((t (:background "#003366"))))
 '(neo-banner-face ((t :inherit shadow)))
 '(neo-button-face ((t :inherit dired-directory)))
 '(neo-dir-link-face ((t :inherit dired-directory)))
 '(neo-expand-btn-face ((t :inherit button)))
 '(neo-file-link-face ((t :inherit default)))
 '(neo-header-face ((t :inherit shadow)))
 '(neo-root-dir-face ((t :inherit link-visited :underline nil)))
 '(sp-pair-overlay-face ((t (:background "grey20"))))
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-current-element-highlight-face ((t (:foreground "#FF8A4B"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#FFE84B"))))
 '(web-mode-html-tag-face ((t (:foreground "#729fcf"))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))
