;;; package --- org mode configuration
;;;
;;; Commentary:
;;;
;;; Filename   : org-config.el
;;; Description: ORG mode configuration and customization
;;               Predominantly used for pdf docs and presentations
;;
;; shell command execution example (C-c C-c) also include in export
;; #+begin_src sh :results output :exports both
;;   df -Ph
;; #+end_src
;;
;; Installation of minted.sty
;; In order to have that tex convert to pdf, you have to ensure that you have
;; minted.sty in your TEXMF folder.
;;  - To know if minted.sty in correct path do "kpsewhich minted.sty".
;;  - If it is not found, download from http://www.ctan.org/tex-archive/macros/latex/contrib/minted
;;  - Generate minted.sty by "tex minted.ins"
;;  - To know your TEXMF folder, do "kpsewhich -var-value=TEXMFHOME"
;;  - if folder is ~/texmf
;;  - Move the minted.sty to your $TEXMF/tex/latex/commonstuff folder.
;;  - Do mkdir -p ~/texmf/tex/latex/commonstuff if that folder hierarchy doesn't exist
;;  - Do "mktexlsr" to refresh the sty database
;;  - Generate pdf from the Org exported tex by "pdflatex -shell-escape FILE.tex"
;;
;; help https://nakkaya.com/2010/09/07/writing-papers-using-org-mode/
;;      https://orgmode.org/worg/org-tutorials/org-latex-export.html
;;      https://www.sharelatex.com/learn
;;      https://en.wikibooks.org/wiki/LaTeX
;;
;; elisp code for org mode configuration support and handling
;;
;;; Code:
;;;
;;;============================================================================
(require 'org)
(require 'org-install)
(require 'org-easy-img-insert)
(require 'ox-latex)
(require 'ox-html)
(require 'ox-reveal)
(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-org)
(require 'ox-ascii)

(after 'org

  ;; -- basic variable setup
  ;; == file mode association
  ;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  (setq org-log-done t)
  (setq org-log-into-drawer t)

  ;; == do not use auto-fill in org-mode
  (auto-fill-mode -1)
  (remove-hook 'text-mode-hook #'turn-on-auto-fill)

  ;; == make editing invisible regions smart
  (setq org-catch-invisible-edits 'smart)

  ;; == allow lists with letters in them
  (setq org-list-allow-alphabetical t)

  ;; == setup archive location in archive directory in current folder
  (setq org-archive-location "archive/%s_archive::")

  ;; -- miscellaneous settings for org-mode
  ;; == renumber the footnotes when new footnotes are inserted
  (setq org-footnote-auto-adjust t)

  ;; eye candies, indentation and font coloring in code blocks
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)                             ;; default with the images open
  (setq org-image-actual-width nil)

  ;; display wrapped lines instead of truncated lines
  (setq truncate-lines nil)
  (setq word-wrap t)

  (setq org-indent-indentation-per-level 2)
  (setq org-src-preserve-indentation t)                               ;; no extra indentation in the source blocks
  (setq org-src-fontify-natively t)
  (setq org-fontify-done-headline t)
  (setq org-support-shift-select 'always)
  (setq org-src-tab-acts-natively t)                                  ;; have completion in blocks
  (setq org-confirm-babel-evaluate nil)                               ;; coding without prompt
  (setq org-hide-leading-stars t)
  (setq org-alphabetical-lists t)
  (setq org-hide-emphasis-markers t)                                  ;; hide the *,=, or / markers
  (setq org-pretty-entities t)                                        ;; have \alpha, \to and others display as utf8
  (setq org-confirm-elisp-link-function nil)
  (setq org-confirm-shell-link-function nil)

  ;; fast TODO selection
  (setq org-use-fast-todo-selection t)
  ;; change a state using C-c C-t KEY where KEY is one of org-todo-keywords
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  turn on visual-line-mode for Org-mode only                             ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; install adaptive-wrap
  (require-package 'adaptive-wrap)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              ;; make the lines in the buffer wrap around the edges of the screen.
              ;; to press C-c q  or fill-paragraph ever again!
              ;; (visual-line-mode 1)
              (org-indent-mode t))
            t)

  ;;{{{
  ;;  auto complete sources for org
  (require-package 'org-ac)
  ;;(require 'org-ac)
  (org-ac/config-default)
  ;;}}}

  ;;{{{
  ;; ox-beamer export - allow for export=>beamer by placing
  ;; #+latex_class: beamer in Org files
  (after "ox-latex"
    ;; https://github.com/fniessen/refcard-org-beamer
    ;; update the list of LaTeX classes and associated header (encoding, etc.)
    ;; and structure
    '(add-to-list 'org-latex-classes
                  `("beamer"
                    ,(concat "\\documentclass[presentation]{beamer}\n"
                             "[DEFAULT-PACKAGES]"
                             "[PACKAGES]"
                             "[EXTRA]\n")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  ;;}}}

  ;;{{{ for thesis
  (add-to-list 'org-latex-classes
               '("thesis" "\\documentclass{thesis}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


  (add-to-list 'org-latex-classes
               '("documentation" "
\\NeedsTeXFormat{LaTeX2e}
\\documentclass[a4paper,10pt,twoside,openright,numbers=noenddot,headings=normal]{scrbook}
[NO-DEFAULT-PACKAGES]

% default packages (without inputenc, because we use xetex)
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\tolerance=1000

% Encoding
\\usepackage[ngerman,english]{babel}
\\usepackage{fontspec}
\\usepackage{polyglossia}

% Fonts
\\setmainfont{Source Serif Pro}
\\setsansfont{Source Sans Pro}
\\setromanfont{Source Sans Pro}
\\setmonofont{Source Code Pro}[Scale=MatchLowercase]

% Default packages
\\usepackage{multirow}                  % Table rows multiline
\\usepackage{graphicx}
\\usepackage{verbatim}
\\usepackage{subfigure}
\\usepackage{url}
\\usepackage{amssymb}
\\usepackage{amsmath}
% biblio
\\usepackage{cite}

% Layout
\\usepackage[scale=0.70, marginratio={4:5, 3:4}, ignoreall, headsep=8mm]{geometry}
\\setlength{\\parskip}{1.4ex plus 0.35ex minus 0.3ex}
\\renewcommand\\arraystretch{1.3}       % stretch lines in tables
\\clubpenalty10000                      % no orphan lines
\\widowpenalty10000                     % no widowed lines
\\setcounter{tocdepth}{3}               % max depth of in toc

% Header and Footer
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhead[RO]{\\slshape \\rightmark}
\\fancyhead[LE]{\\slshape \\leftmark}
\\fancyhead[LO,RE]{}
\\fancyheadoffset[L,R]{0.5cm}
\\fancypagestyle{plain}{
\\fancyhf{}                           % clear all header and footer fields
\\fancyfoot[C]{\\thepage}             % except the center
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}}

\\usepackage{hyperref}
\\hypersetup{
colorlinks=false,
pdfborder=0 0 0                       % no boxes on links
}
"

           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;}}}

;;{{{ for using the publication format for copernicus
;; move the files under latex/copernicus of vendor-dir to
;; the directory where org paper is being created
;; use #+Latex_Class: copernicus_discussions
;; #+LaTeX_CLASS_OPTIONS: [acpd, hvmath, online]
(after 'ox-latex
  (add-to-list 'org-latex-classes
               `("copernicus_discussions"
                 "\\documentclass{copernicus_discussions}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  ;;}}}


  ;;{{{
  ;;  inserting code blocks
  (defun org-insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in `org-mode'."
    (interactive
     (let ((src-code-types
            '("emacs-lisp"
              "python"
              "C"
              "shell"
              "java"
              "js"
              "clojure"
              "C++"
              "css"
              "dot"
              "gnuplot"
              "plantuml"
              "awk"
              "sh"
              "lisp"
              "ditaa"
              "haskell"
              "latex"
              "org"
              "erlang")))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (progn
      (newline-and-indent)
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC\n")
      (previous-line 2)
      (org-edit-src-code)))
  ;;-----------------------
  ;;}}}

  ;;{{{
  ;; Notes - press M-x org-insert-src-block , then type in the wanted Emacs
  ;;         major mode for the block, for example, emacs-lisp (press TAB to
  ;;         do auto-completion)
  ;; Bind C-c s i to the function above
  (add-hook 'org-mode-hook
            '(lambda ()
               ;; turn on flyspell-mode by default
               (flyspell-mode 1)
               ;; C-TAB for expanding
               (local-set-key (kbd "C-<tab>")
                              'yas/expand-from-trigger-key)
               ;; keybinding for editing source code blocks
               (local-set-key (kbd "C-c s e")
                              'org-edit-src-code)
               ;; keybinding for inserting code blocks
               (local-set-key (kbd "C-c s i")
                              'org-insert-src-block)))
  ;;}}}



  ;;{{{ org workline
  (setq org-todo-keywords
        '((sequence "☛ TODO(t)" "₪ NEXT(n@)" "|" "✔ DONE(d@)")
          (sequence "⚑ WAITING(w@/!)" "⟁ HOLD(h@/!)" "|")
          (sequence "|" "✘ CANCELED(c@/!)" "|" "§ POSTPONED(p@/!)" "PHONE" "MEETING")))
  ;;}}}


  ;;{{{
  ;; State Triggers for TODO
  ;; The triggers are governed by the following rules
  ;; Moving a task to CANCELLED adds a CANCELLED tag
  ;; Moving a task to WAITING adds a WAITING tag
  ;; Moving a task to HOLD adds WAITING and HOLD tags
  ;; Moving a task to a done state removes WAITING and HOLD tags
  ;; Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
  ;; Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
  ;; Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  ;;}}}


  ;;{{{ TODO Keyword Faces
  (setq org-todo-keyword-faces
        '(("TODO"      . (:foreground "green"        :weight bold))
          ("IDEA"      . (:foreground "GoldenRod"    :weight bold))
          ("NEXT"      . (:foreground "IndianRed1"   :weight bold))
          ("DONE"      . (:foreground "forest green" :weight bold))
          ("STARTED"   . (:foreground "OrangeRed"    :weight bold))
          ("WAITING"   . (:foreground "IndianRed1"   :weight bold))
          ("HOLD"      . (:foreground "magenta"      :weight bold))
          ("CANCELLED" . (:foreground "LimeGreen"    :weight bold))
          ("POSTPONED" . (:foreground "LimeGreen"    :weight bold))
          ("MEETING"   . (:foreground "forest green" :weight bold))
          ("PHONE"     . (:foreground "forest green" :weight bold))))
  ;;}}}

  ;;{{{
  ;; If you have a preferred set of tags that you would like to use in every file,
  ;; in addition to those defined on a per-file basis by TAGS option lines, then you
  ;; may specify a list of tags with this variable
  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("HOME"      . ?h)
          ("RESEARCH"  . ?r)
          ("TEACHING"  . ?t)
          (:endgroup   . nil)
          (:startgroup . nil)
          ("OS"        . ?o)
          ("DEV"       . ?d)
          ("WWW"       . ?w)
          (:endgroup   . nil)
          ("URGENT"    . ?u)
          ("KEY"       . ?k)
          ("HARD"      . ?a)
          ("BONUS"     . ?b)
          ("noexport"  . ?x)))
  ;;}}}


  ;;{{{ specify special faces for specific tags using the option org-tag-faces
  (setq org-tag-faces
        '(("HOME"     . (:foreground "DarkGoldenRod1" :weight bold))
          ("RESEARCH" . (:foreground "DarkGoldenRod1" :weight bold))
          ("TEACHING" . (:foreground "DarkGoldenRod1" :weight bold))
          ("OS"       . (:foreground "IndianRed1" :weight bold))
          ("DEV"      . (:foreground "IndianRed1" :weight bold))
          ("WWW"      . (:foreground "IndianRed1" :weight bold))
          ("URGENT"   . (:foreground "Red" :weight bold))
          ("KEY"      . (:foreground "Red" :weight bold))
          ("HARD"     . (:foreground "Red" :weight bold))
          ("BONUS"    . (:foreground "DarkGoldenRod1" :weight bold))
          ("noexport" . (:foreground "Red" :weight bold))))
  ;;}}}

  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido t)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images) ;; update images from babel code blocks automatically

  (defun /org/org-mode-hook ()
    (toggle-truncate-lines t)
    (setq show-trailing-whitespace t))
  (add-hook 'org-mode-hook #'/org/org-mode-hook)

  ;; Asynchronous src_block execution for org-babel
  (require-package 'ob-async)
  (require 'ob-async)
  (add-to-list 'org-ctrl-c-ctrl-c-hook #'ob-async-org-babel-execute-src-block)

  ;;{{{ org mode beautification
  ;; change the elipsis face
  ;; (setq org-ellipsis "⚡⚡⚡")

  ;;  org bullets for markdown
  ;;  use org-bullets-mode for utf8 symbols as org bullets
  ;;  select, do [M-x eval-region]. The *s will be replaced with utf-8 bullets
  ;;  next time you open an org file
  (require-package 'org-bullets)
  ;;(setq org-bullets-bullet-list '("●" "○" "◆" "◇" "▸"))
  (add-hook 'org-mode-hook #'org-bullets-mode)
  ;;}}

  ;;{{{  some custom settings and todo configuration from pragmatic emacs
  (setq org-list-description-max-indent 5)    ;; set max indentation for description lines
  (setq org-adapt-indentation nil)            ;; prevent demoting heading, shifting text in sections
  ;;}}}

  ;;{{{ for src code syntax highlighting during export
  ;; install python pygments package
  ;; this is for code syntax highlighting in export. you need to use
  ;; -shell-escape with latex, and install pygments.
  (setq org-latex-listings t)   ;; for a nice looking code block, use Listings instead of Verbatim
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "parskip"))
  ;;}}}


  ;;{{{ get listings to wrap
  ;; (setq org-latex-listings-options '(("breaklines" "true")))
  (setq org-latex-listings-options '(("breaklines" "true")
                                     ("literate" "{0}{0}{1}%
{1}{1}{1}%
{2}{2}{1}%
{3}{3}{1}%
{4}{4}{1}%
{5}{5}{1}%
{6}{6}{1}%
{7}{7}{1}%
{8}{8}{1}%
{9}{9}{1}%
")))
  ;;}}}

  ;;{{{ minted options through pygments
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("bgcolor" "mintedbg")
          ("mathescape" "true")
          ("linenos" "")
          ("breaklines" "true")
          ("breakanywhere" "true")))
  ;;}}}


 ;;{{{ org pdf export options
 ;;    using xelatex instead of pdflatex as fontspec if only supported by
 ;;    xelatex or lualatex
 (setq org-latex-pdf-process
       '(
         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ;;"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ;;"biber %b"
         ;;"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ;;"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ;;"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ;;"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ;;"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         ))
 ;;}}}


 ;;{{{ spell check -  making ispell work with the org-mode
 (defun org-ispell ()
   "Configure `ispell-skip-region-alist' for `org-mode'."
   (make-local-variable 'ispell-skip-region-alist)
   (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
   (add-to-list 'ispell-skip-region-alist '("~" "~"))
   (add-to-list 'ispell-skip-region-alist '("=" "="))
   (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
 (add-hook 'org-mode-hook #'org-ispell)
 ;;}}}


 ;;{{{ org babel loads
 (lambda()
   (require 'ob-emacs-lisp)
   (require 'ob-http)
   (require 'ob-latex)
   (require 'ob-haskell)
   (require 'ob-elixir)
   (require 'ob-scala)
   (require 'ob-python)
   (require 'ob-ipython)
   (require 'ob-C)
   (require 'ob-clojure)
   (require 'ob-shell)
   (require 'ob-org)
   (require 'ob-awk)
   (require 'ob-sed)
   (require 'ob-css)
   (require 'ob-js)
   (setq org-export-babel-evaluate nil)            ;; do not export code on export by default
   (setq org-imenu-depth 3)                        ;; increase imenu depth to include third level headings
   (setq org-babel-python-command "python3")       ;; set python3 as default
   ;; Out of the box Emacs supports js with js-mode.
   ;; define language javascript to use js2-mode
   (add-to-list 'org-src-lang-modes '("javascript" . js2)))
 ;;}}}


 ;;{{{ remove key bindings for some motions
 (defun my-org-mode-hook ()
   "Remove unnecessary keymaps."
   (let ((oldmap (cdr (assoc 'evil-motion-state-minor-mode minor-mode-map-alist)))
         (newmap (make-sparse-keymap)))
     (set-keymap-parent newmap oldmap)
     (define-key newmap (kbd "\\") nil)
     (define-key newmap (kbd "*") nil)
     (define-key newmap (kbd "SPC") nil)
     (make-local-variable 'minor-mode-overriding-map-alist)
     (push `(evil-motion-state-minor-mode . ,newmap) minor-mode-overriding-map-alist)))

 (add-hook 'org-mode-hook 'my-org-mode-hook)
 ;;}}}


 ;;{{{ Load Helper and utilities configuration
 (load-file (concat module-dir "/org-helper-config.el"))
 ;;}}}

 ;;{{{ use imagemagick to preview latex
 ;; (setq org-latex-create-formula-image-program 'imagemagick)
 (setq org-preview-latex-default-process 'imagemagick)
 ;;}}}

 ;;{{{ use pre-evaluated results directly and avoid repeated evaluation again
 ;; during the insertion of src templates in pdf export
 (setq org-export-babel-evaluate 'inline-only)
 ;;}}}


 ;;{{{ Prettier (or at least fancier) code block delimiters
 (defun prettier-org-code-blocks ()
   "For prettier code blocks."
   (interactive)
   (font-lock-add-keywords nil
                           '(("\\(\+begin_src\\)"
                              (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                                        nil)))
                             ("\\(\+end_src\\)"
                              (0 (progn (compose-region (match-beginning 1) (match-end 1) ?¦)
                                        nil))))))
 (add-hook 'org-mode-hook #'prettier-org-code-blocks)
 ;;}}}


 ;;{{{ org babel
 (require-package 'ob-restclient)       ;; org-mode extension to restclient.el
 (require-package 'ob-elixir)           ;; org-mode extension to ipython
 (require-package 'ob-ipython)          ;; org-mode extension to elixir/erlang
 ;; execute the query by pressing C-c C-c on the source-block header
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((python     . t)
    (emacs-lisp . t)
    (coq        . t)
    (awk        . t)
    (haskell    . t)
    (elixir     . t)
    (clojure    . t)
    (calc       . t)
    (ledger     . t)
    (ditaa      . t)
    (plantuml   . t)
    ;;(sh       . t)
    (shell      . t)
    (scala      . t)
    (js         . t)
    (java       . t)
    (sql        . t)
    (dot        . t)
    (http       . t)
    (org        . t)
    (latex      . t)
    (restclient . t)))

  ;; Upcase #+begin_example...#+end_example in the results
  (setq org-babel-uppercase-example-markers t)

 ;;}}}

 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; org-config.el ends here
