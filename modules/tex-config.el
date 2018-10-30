;;; package --- tex-config.el configuration settings for LaTeX/AucTex
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : tex-config.el
;;; Description: Emacs Color theme
;;; Additional Packages: https://en.wikibooks.org/wiki/LaTeX/Installing_Extra_Packages
;;; Help: https://github.com/grettke/help
;;;       https://en.wikibooks.org/wiki/User:Dirk_H%C3%BCnniger/latex
;;;       https://edwardtoday.wordpress.com/2012/12/09/notes-on-typesetting-my-thesis-with-latex/
;;;
;;; Tex package documentation at
;;;       http://www.texdoc.net/
;;;
;;; elisp code for customizing the latex
;;;
;;; Code:
;;;
;;===========================================================================
(require 'latex-pretty-symbols)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))

;; set the PATH for texbin
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; set the LaTeX Engine
(setq TeX-engine (quote "/Library/TeX/texbin/xetex"))
;; add synctex
(setq LaTeX-command "pdflatex -synctex=1")

;; for AucTex
;; revert the PDF-buffer after the TeX compilation has finished
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;;
;; TeX Completion with Company
(after "company"
  (require-package 'company-auctex)
  (add-hook 'LaTeX-mode-hook #'company-auctex-init))

;;
;; TeX Cmpletion with Auto-Complete
(after "auto-complete"
  (require-package 'auto-complete-auctex))


;; TeX command list configuration
(setq TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("LaTeX Make" "latexmk -pdflatex='pdflatex -synctex=1' -pdf %s" TeX-run-command nil t
      :help "Run LaTeX Make")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber t t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("XeLaTeX_SyncteX" "%`xelatex --synctex=1%(mode)%' %t " TeX-run-command nil
      (latex-mode doctex-mode)))))

;;
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)


;; PDF with LaTeX by default
(defun TeX-PDF-mode-on ()
  "Set the TEX PDF mode on."
  (interactive)
  (TeX-PDF-mode 1))

(add-hook 'tex-mode-hook 'TeX-PDF-mode-on)
(add-hook 'latex-mode-hook 'TeX-PDF-mode-on)

(setq reftex-plug-into-AUCTeX t)
;; (setq TeX-PDF-mode t)

;; auto-fill mode
(defun auto-fill-mode-on ()
  "Turn on Autofill mode."
  (auto-fill-mode 1))

(add-hook 'text-mode-hook 'auto-fill-mode-on)
(add-hook 'emacs-lisp-mode 'auto-fill-mode-on)
(add-hook 'tex-mode-hook 'auto-fill-mode-on)
(add-hook 'latex-mode-hook 'auto-fill-mode-on)


;;{{{ for citations using reftex
(after 'reftex
  (require 'reftex-cite)
  (defun org-mode-reftex-setup ()
    (interactive)
    (and (buffer-file-name) (file-exists-p (buffer-file-name))
         (progn
           ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
           (setq TeX-master t)
           (turn-on-reftex)
           ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
           (global-auto-revert-mode t) ; careful: this can kill the undo
                                        ; history when you change the file
                                        ; on-disk.
           (reftex-parse-all)
           ;; add a custom reftex cite format to insert links
           ;; This also changes any call to org-citation!
           (reftex-set-cite-format
            '((?c . "\\citet{%l}") ; natbib inline text
              (?i . "\\citep{%l}") ; natbib with parens
              ))))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
    (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

  (add-hook 'org-mode-hook 'org-mode-reftex-setup))
;;}}}


;; (setq TeX-output-view-style
;;       (quote
;;        (("^pdf$" "." "evince -f %o")
;;         ("^html?$" "." "iceweasel %o"))))

;; Use Skim as viewer, enable source <-> PDF sync
(dolist (dir '("/Applications/Skim.app/Contents/SharedSupport"))
  (add-to-list 'exec-path dir))

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "open -a Skim.app %o")
        ("^dvi$" "^xdvi$" "open-x11 %(o?)xdvi %dS %d")
        ("^dvi$" "^TeXniscope$" "open -a TeXniscope.app %o")
        ("^pdf$" "." "open %o")
        ("^html?$" "." "open %o"))))

;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (push
                              '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                :help "Run latexmk on file")
                              TeX-command-list)))
(add-hook 'TeX-mode-hook
          '(lambda ()
             (setq TeX-command-default "latexmk")))

;; Pdf Viewer Settings
;; use Skim as default pdf viewer
(add-hook 'LaTeX-mode-hook
      (lambda()
        (add-to-list 'TeX-expand-list
             '("%q" skim-make-url))))

(defun skim-make-url ()
  (concat
   (TeX-current-line)
   " \""
   (expand-file-name (funcall file (TeX-output-extension) t)
                     (file-name-directory (TeX-master-file)))
   "\" \""
   (buffer-file-name)
   "\""))

;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-list
      '(("Preview.app" "open -a Preview.app %o")
        ("PDF Viewer" "open -a Skim.app %o")
        ("displayline" "displayline -g -b %n %o %b")
        ("open" "open %o"))
      TeX-view-program-selection
      '((output-dvi "open")
        (output-pdf "PDF Viewer")
        (output-html "open")))

;; hide some parts of the text file
(defun turn-on-outline-minor-mode ()
  "Hide or show parts of a text file."
  (outline-minor-mode 1))

;; shortcuts C-c C-o C-l | C-c C-o C-n | C-c C-o C-p | C-c C-o C-a
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX export settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpret "_" and "^" for export when braces are used
(setq org-export-with-sub-superscripts '{})

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
        ("" "lmodern" nil)
        ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "amssymb" t)
        ("" "amsmath" t)
        ("version=3" "mhchem" t)
        ("numbers,super,sort&compress" "natbib" nil)
        ("" "natmove" nil)
        ("" "url" nil)
        ("" "minted" nil)
        ("" "underscore" nil)
        ("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
         "hyperref" nil)
        ("" "attachfile" nil)))

;; do not put in \hypersetup. Use your own if you want it e.g.
;; \hypersetup{pdfkeywords={%s},\n pdfsubject={%s},\n pdfcreator={%}}
(setq org-latex-with-hyperref nil)

(setq org-export-latex-listings t)

;; avoid getting \maketitle right after begin{document}
;; you should put \maketitle if and where you want it.
;; (setq org-latex-title-command "")

(setq org-latex-prefer-user-labels t)

(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex, pdflatex or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex=xelatex -pdf %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd))
  ; (setq org-latex-to-pdf-process
  ;       '("xelatex -interaction nonstopmode %f"
  ;         "xelatex -interaction nonstopmode %f")) ;; for multiple passes
  )

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)
;; Default packages included in every tex file, pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-export-latex-default-packages-alist
        '(("AUTO" "inputenc" t)
          ("T1"   "fontenc"   t)
          (""     "fixltx2e"  nil)
          (""     "wrapfig"   nil)
          (""     "soul"      t)
          (""     "textcomp"  t)
          (""     "marvosym"  t)
          (""     "wasysym"   t)
          (""     "latexsym"  t)
          (""     "amssymb"   t)
          (""     "hyperref"  nil)))

  ;; Packages to include when xelatex is used
  ;; (see https://github.com/kjhealy/latex-custom-kjh for the
  ;; non-standard ones.)
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-default-packages-alist
            '(("" "fontspec" t)
              ("" "xunicode" t)
              ("" "url" t)
              ("" "rotating" t)
              ("" "memoir-article-styles" t)
              ("american" "babel" t)
              ("babel" "csquotes" t)
              ("" "listings" nil)
              ("" "listings-sweave-xelatex" nil)
              ("svgnames" "xcolor" t)
              ("" "soul" t)
              ("xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels, bookmarksnumbered" "hyperref" nil)
              )))

  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-classes
            (cons '("article"
                    "\\documentclass[11pt,article,oneside]{memoir}
        \\input{vc}
        \\usepackage[style=authoryear-comp-ajs, abbreviate=true]{biblatex}
        \\bibliography{socbib}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                  org-export-latex-classes))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)


;;** LaTeX preview pane
;;**  minor mode for Emacs that enables you to preview your LaTeX
;;**  files directly in Emacs
(use-package latex-preview-pane
  :defer t)

;;** FlyMake integration
(defun flymake-get-tex-args (file-name)
  "FILE-NAME against which FlyMake should run."
  (list "pdflatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tex-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; tex-config.el ends here
