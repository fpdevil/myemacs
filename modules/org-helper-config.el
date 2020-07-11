;; File              : org-helper-config.el
;; Author            : Sampath Singamsetty <Singamsetty.Sampat@gmail.com>
;; Date              : 31.05.2019
;; Last Modified Date: 31.05.2019
;; Last Modified By  : Sampath Singamsetty <Singamsetty.Sampat@gmail.com>
;;; package --- org helpers configuration -*- lexical-binding:t ; -*-
;;;
;;; Commentary:
;;;
;;; Filename   : org-helper-config.el
;;; Description: Some helper functions and utils for org-mode
;;
;; elisp code for org helpers and utilities
;; Examples using Emacs org mode babel inline source code with
;; different backend languages
;; https://github.com/dfeich/org-babel-examples
;; http://ehneilsen.net/notebook/orgExamples/org-examples.html
;; https://texblog.org/2017/12/12/color-table-series-part-1-introduction-colortbl-package/
;;;
;;; Code:
;;;

(defun s-org-mode-hook ()
  "Aiding capf completions through company."
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'s-org-mode-hook)

;;-----------------------------------------------------------------------------
;; enable flycheck for babel source code blocks
;;-----------------------------------------------------------------------------
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  "Enable FlyCheck AROUND SET-BUFFER-FILE-NAME ACTIVATE COMPILE."
  (let ((file-name (buffer-file-name))) ;; (1)
    ad-do-it                            ;; (2)
    (setq buffer-file-name file-name))) ;; (3)

;;-----------------------------------------------------------------------------
;; for better bullet point handling, use font-lock
;;-----------------------------------------------------------------------------
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;-----------------------------------------------------------------------------
;; for prettifying the check boxes
;;-----------------------------------------------------------------------------
(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

;;------------------------------------------------------------------------------
;; for lists (https://orgmode.org/manual/Plain-lists.html)
;; If you  find that using a  different bullet for a  sub-list (than that
;; used for  the current list-level) improves  readability, customize the
;; variable org-list-demote-modify-bullet
;;------------------------------------------------------------------------------
(setq org-list-demote-modify-bullet (quote (("+" . "-")
					    ("*" . "-")
					    ("1." . "-")
					    ("1)" . "a)"))))

;;------------------------------------------------------------------------------
;; adjust scale of formulas or preview objects change value of scale to 2.0
;;------------------------------------------------------------------------------
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;;-----------------------------------------------------------------------------
;; org agenda files
;;-----------------------------------------------------------------------------
(setq org-directory (expand-file-name "org" personal-dir))
(setq org-agenda-files (list (expand-file-name "home.org" org-directory)
			     			 (expand-file-name "python.org" org-directory)
			     			 (expand-file-name "todo.org" org-directory))
      org-agenda-include-all-todo t
      org-agenda-include-diary t
      org-agenda-inhibit-startup t                ;; this speeds up by ~50x
      org-agenda-use-tag-inheritance nil)


;;-----------------------------------------------------------------------------
;; Refile targets include this file and any file contributing
;; to the agenda - up to 5 levels deep
;;-----------------------------------------------------------------------------
(setq org-refile-targets
      (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB
;; shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;;-----------------------------------------------------------------------------
;;**  Image handling in org
;;    Display images in org mode enable image mode first
;;-----------------------------------------------------------------------------
(iimage-mode)

;; add the org file link format to the iimage mode regex
(add-to-list 'iimage-mode-image-regex-alist
	     (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]")  1))

;;  add a hook so we can display images on load
(add-hook 'org-mode-hook
  '(lambda () (org-turn-on-iimage-in-org)))

;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (iimage-mode)
  ;; (turn-on-iimage-mode)
  (set-face-underline 'org-link nil))

;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline 'org-link nil)
    (set-face-underline 'org-link t))
  (call-interactively 'iimage-mode))

(define-key org-mode-map (kbd "C-S-a") 'org-archive-subtree)


;;-----------------------------------------------------------------------------
;;** Drag images and files onto org-mode and insert a link to them
;;   inserting images from web M-x org-easy-img-insert
;;
;; http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/
;; Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/
;;-----------------------------------------------------------------------------
(require 'org-download)
(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
	 (type (car payload))
	 (fname (cadr payload))
	 (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
	    (eq 'file type)
	    (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
	    (eq 'file type)
	    (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
	    (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
	   (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))

(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)


;;-----------------------------------------------------------------------------
;; pdf tools for viewing and interacting with pdf
;;-----------------------------------------------------------------------------
(setenv "PKG_CONFIG_PATH"
        (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"") "/lib/pkgconfig/"))
(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  ;; open pdfs scaled to the fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; auto annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use regular isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq mouse-wheel-follow-mouse t)
  ;;  more fine grained zooming with + and - than the default 25%, now 10%
  (setq pdf-view-resize-factor 1.10)
  ;; cua mode interferes with copying text from pdf
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0))))


;;-----------------------------------------------------------------------------
;; org-mode modules for citations, cross-references, bibliographies in the
;; org-mode and useful bibtex tools to go with it
;;-----------------------------------------------------------------------------
(use-package org-ref
  :ensure t
  :init
  (setq reftex-default-bibliography '("~/.emacs.d/personal/org/resources/bibliography/references.bib")
        org-ref-bibliography-notes '("~/.emacs.d/personal/org/resources/bibliography/notes.org")
        org-ref-default-bibliography '("~/.emacs.d/personal/org/resources/bibliography/references.bib")
        org-ref-pdf-directory '("~/.emacs.d/personal/org/resources/bibliography/bibtex-pdfs/")
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5
        org-ref-bibtex-hydra-key-binding (kbd "H-b"))
  ;; (define-key bibtex-mode-map org-ref-bibtex-hydra-key-binding 'org-ref-bibtex-hydra/body)
  ;; (global-set-key (kbd "H-b") 'org-ref-bibtex-hydra/body)
  )


;; various functions useful for bibliography management
(defun my-bibliography-selector-hook (backend)
  (case backend
    (latex
     (when (save-excursion
             (re-search-forward "^[ \t]*\\bibliography\\(?:style\\)?{" nil t))
       (while (re-search-forward "^[ \t]*#+BIBLIOGRAPHY:.*$" nil t)
         (when (eq (org-element-type (save-match-data (org-element-at-point)))
                   'keyword)
           (replace-match "")))))
    (html
     (when (save-excursion
             (re-search-forward "^[ \t]*#+BIBLIOGRAPHY:.*$" nil t))
       (while (re-search-forward "^[ \t]*\\bibliography\\(?:style\\)?{.*$" nil t)
         (replace-match ""))))))

(add-hook 'org-export-before-parsing-hook 'my-bibliography-selector-hook)

(require 'reftex)
(require-package 'bibretrieve)
(setq bibretrieve-backends '(("citebase" . 10) ("mrl" . 10) ("arxiv" . 5) ("zbm" . 5)))

(defun bibretrieve-amazon-create-url (author title)
  (concat "http://lead.to/amazon/en/?key="(mm-url-form-encode-xwfu title) "&si=ble&op=bt&bn=&so=sa&ht=us"))

(defun bibretrieve-amazon ()
  (interactive)
  (setq mm-url-use-external t)
  (setq mm-url-program "w3m")
  (setq mm-url-arguments (list "-dump"))
  (setq bibretrieve-backends '(("amazon" . 5)))
  (bibretrieve)
  (setq mm-url-use-external nil))


;;-----------------------------------------------------------------------------
;;** allow for export=>beamer by placing
;;   http://emacs-fu.blogspot.com/2009/10/writing-presentations-with-org-mode-and.html
;;   http://mirror.utexas.edu/ctan/macros/latex/contrib/beamer/doc/beameruserguide.pdf
;; #+LaTeX_CLASS: beamer in org files
;;-----------------------------------------------------------------------------
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
	     ;; beamer class, for presentations
	     '("beamer"
	       "\\documentclass[11pt,professionalfonts]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\usepackage{multirow}
      \\usepackage{subfigure}
      \\usepackage{graphicx}
      \\usepackage{xcolor}
      \\usepackage{url}
      \\usepackage{amssymb}
      \\usepackage{amsmath}
      \\usepackage{tikz}
      \\usepackage{xcolor}
      \\usepackage{amsmath}
      \\usepackage{lmodern}
      \\usepackage[margin=1in]{geometry}
      \\usepackage{algorithmic}
      \\usepackage{algorithm}
      \\usepackage{fontspec,xunicode,xltxtra}
      \\usepackage{polyglossia}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}"
		"\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}")))

;; letter class, for formal letters
(add-to-list 'org-export-latex-classes
	     '("letter"
	       "\\documentclass[11pt]{letter}\n
		    \\usepackage[utf8]{inputenc}\n
		    \\usepackage[T1]{fontenc}\n
		    \\usepackage{color}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; private latex classes (referred from http://www.star.bris.ac.uk/bjm/org-basics.html)
(add-to-list 'org-latex-classes
	     '("bjmarticle"
	       "\\documentclass{article}
			\\usepackage[utf8]{inputenc}
			\\usepackage[T1]{fontenc}
			\\usepackage{graphicx}
			\\usepackage{longtable}
			\\usepackage{hyperref}
			\\usepackage{natbib}
			\\usepackage{amssymb}
			\\usepackage{amsmath}
			\\usepackage{geometry}
			\\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;-----------------------------------------------------------------------------
;;** Wrap Region
;;-----------------------------------------------------------------------------
(use-package wrap-region
  :diminish wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil (org-mode))
     ("~" "~" nil (org-mode))
     ("/" "/" nil (org-mode))
     ("=" "=" "+" (org-mode))
     ("_" "_" nil (org-mode))
     ("$" "$" nil (org-mode latex-mode)))
   (add-hook 'org-mode-hook 'wrap-region-mode)
   (add-hook 'latex-mode-hook 'wrap-region-mode)))


;;-----------------------------------------------------------------------------
;; utilities to export scientific manuscripts
;;-----------------------------------------------------------------------------
;; (use-package ox-manuscript
;;   :after org
;;   :ensure t
;;   :load-path (lambda () (expand-file-name "ox-manuscript" vendor-dir)))

;;-----------------------------------------------------------------------------
;;** swagger file to org
;; M-x swagger-to-org-from-file-name
;;-----------------------------------------------------------------------------
(use-package swagger-to-org
  :after org
  :defer t)

;;-----------------------------------------------------------------------------
;;** custom faces
;;-----------------------------------------------------------------------------
(custom-theme-set-faces
 'user
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-document-info         ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link                  ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))

;;-----------------------------------------------------------------------------
;;** clear org cache
;;-----------------------------------------------------------------------------
(defun clear-org-cache ()
  "Clears all kinds of Org-mode caches and re-builds them if possible."
  (interactive)
  (measure-time
   (org-element-cache-reset)
   (org-refile-cache-clear)
   (org-refile-get-targets)
   (setq org-agenda-tags-column (- (- (window-total-width) 3))) ;; total width minus 3
   (when (my-buffer-exists "*Org Agenda*")
     (kill-buffer "*Org Agenda*")
     (org-agenda-list))))

;;-----------------------------------------------------------------------------
;;** reformat the buffer
;;-----------------------------------------------------------------------------
(defun org-reformat-buffer ()
  ("Reformat the org buffer.")
  (interactive)
  (when (y-or-n-p "Dow you really want to format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

;;-----------------------------------------------------------------------------
;;** publishing a website using org-mode
;;-----------------------------------------------------------------------------
(require 'ox-publish)
(setq org-html-coding-system 'utf-8-unix)

;;
;;** project tree for publishing (4 directories)
;; www
;; |-- org
;; |-- _org
;; |-- static_html
;; |-- utils
;;
(setq org-publish-project-alist
      '(
        ;; static html content
        ("html-static"
         :base-directory "~/www/static_html/"
         :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m\\|R"
         :include (".htaccess")
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)

        ;; notes from org
        ("org-notes"
         :base-directory "~/www/org/"
         :base-extension "org"
         :publishing-directory "~/public_html/org/"
         :recursive t
         :exclude ".*-reveal\.org"        ;; exclude org-reveal slides
         :publishing-function org-html-publish-to-html
         :headline-levels 2               ;; Just the default for this project.
         :auto-sitemap t                  ;; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ;; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ;; ... with title 'Sitemap'.
         :with-creator nil       ;; Disable the inclusion of "Created by Org" in the postamble.
         :with-email nil         ;; Disable the inclusion of "(your email)" in the postamble.
         :with-author nil        ;; Enable the inclusion of "Author: Your Name" in the postamble.
         :auto-preamble t;       ;; Enable auto preamble
         :auto-postamble t       ;; Enable auto postamble
         :table-of-contents t    ;; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
         :toc-levels 2           ;; Just the default for this project.
         :section-numbers nil    ;; Set this to "t" if you want headings to have numbers.
         :html-head-include-default-style nil ;Disable the default css style
         :html-head-include-scripts nil ;Disable the default javascript snippet
         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.i3s.unice.fr/~malapert/css/worg.min.css\"/>\n<script type=\"text/javascript\" src=\"http://www.i3s.unice.fr/~malapert/js/ga.min.js\"></script>" ;Enable custom css style and other tags
         :html-link-home "index.html"    ;; Just the default for this project.
         :html-link-up "../index.html"   ;; Just the default for this project.
		 )

        ;; static org contents
        ("org-static"
         :base-directory "~/www/org/"
         :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m\\|R"
         :publishing-directory "~/public_html/org/"
         :recursive t
         :publishing-function org-publish-attachment
         :exclude "Rplots.pdf"
         )

        ;; from org-notes
        ("org"
         :components ("org-notes" "org-static" "html-static")
         )

        ;; with html
        ("_org-notes"
         :base-directory "~/www/_org/"
         :base-extension "org"
         :publishing-directory "~/private_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 2    ;; Just the default for this project.
         :auto-preamble t
         :auto-sitemap nil     ;; Do NOT Generate sitemap.org automagically...
         :with-creator nil     ;; Disable the inclusion of "Created by Org" in the postamble.
         :with-email nil       ;; Disable the inclusion of "(your email)" in the postamble.
         :with-author nil      ;; Enable the inclusion of "Author: Your Name" in the postamble.
         :auto-preamble t;     ;; Enable auto preamble
         :auto-postamble t     ;; Enable auto postamble
         :table-of-contents t  ;; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
         :toc-levels 2         ;; Just the default for this project.
         :section-numbers nil  ;; Set this to "t" if you want headings to have numbers.
         :html-head-include-default-style nil ;Disable the default css style
         :html-head-include-scripts nil ;Disable the default javascript snippet
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.i3s.unice.fr/~malapert/css/worg.min.css\"/>" ;Enable custom css style
         )

        ("_org-static"
         :base-directory "~/www/_org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz"
         :publishing-directory "~/private_html/"
         :recursive t
         :publishing-function org-publish-attachment
         :exclude "Rplots.pdf"
         )

        ("_org"
         :components ("_org-notes" "_org-static")
         )
        )
      )


;;-----------------------------------------------------------------------------
;; https://github.com/dangom/writefreely.el
;; *Frictionless* blogging with Org Mode
;;-----------------------------------------------------------------------------
(use-package writefreely
  :after org
  :ensure t
  ;; Authentification token, if wanted.
  ;; Alternatively (setq writefreely-auth-token "00000000-0000-0000-0000-000000000000")
  :config (load-library "writefreely-auth-token.el.gpg"))

(use-package toc-org
  :defer t)


(provide 'org-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; org-helper-config.el ends here
