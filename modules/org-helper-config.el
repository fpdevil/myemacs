;;; package --- org helpers configuration
;;;
;;; Commentary:
;;;
;;; Filename   : org-helper-config.el
;;; Description: Some helper functions and utils for org-mode
;;
;; elisp code for org helpers and utilities
;;;
;;;Code:
;;;
;;;============================================================================

;; * Block templates
;; add <p for python expansion
(add-to-list 'org-structure-template-alist
       '("p" "#+BEGIN_SRC python :results output org drawer\n?\n#+END_SRC"
         "<src lang=\"python\">\n?\n</src>"))

;; add <por for python expansion with raw output
(add-to-list 'org-structure-template-alist
       '("por" "#+BEGIN_SRC python :results output raw\n?\n#+END_SRC"
         "<src lang=\"python\">\n?\n</src>"))

;; add <pv for python expansion with value
(add-to-list 'org-structure-template-alist
       '("pv" "#+BEGIN_SRC python :results value\n?\n#+END_SRC"
         "<src lang=\"python\">\n?\n</src>"))

;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
         "<src lang=\"emacs-lisp\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
       '("ell" "#+BEGIN_SRC emacs-lisp :lexical t\n?\n#+END_SRC"
         "<src lang=\"emacs-lisp\">\n?\n</src>"))

;; add <sh for shell
(add-to-list 'org-structure-template-alist
       '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC"
         "<src lang=\"shell\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
       '("lh" "#+latex_header: " ""))

(add-to-list 'org-structure-template-alist
       '("lc" "#+latex_class: " ""))

(add-to-list 'org-structure-template-alist
       '("lco" "#+latex_class_options: " ""))

(add-to-list 'org-structure-template-alist
       '("ao" "#+attr_org: " ""))

(add-to-list 'org-structure-template-alist
       '("al" "#+attr_latex: " ""))

(add-to-list 'org-structure-template-alist
       '("ca" "#+caption: " ""))

(add-to-list 'org-structure-template-alist
       '("tn" "#+tblname: " ""))

(add-to-list 'org-structure-template-alist
       '("n" "#+name: " ""))

(add-to-list 'org-structure-template-alist
       '("o" "#+options: " ""))

(add-to-list 'org-structure-template-alist
       '("ti" "#+title: " ""))

;; == for table expansions
(loop for i from 1 to 6
      do
      (let ((template (make-string i ?t))
      (expansion (concat "|"
             (mapconcat
        'identity
        (loop for j to i collect "   ")
        "|"))))
  (setf (substring expansion 2 3) "?")
  (add-to-list 'org-structure-template-alist
         `(,template ,expansion ""))))


;; * Colored src blocks
;; based on patches from Rasmus <rasmus@gmx.us>

;; This function overwrites the org-src function to make src blocks be colored again.
(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
LANG is the language of the block.  START and END are positions of
the block.  This function is called by Emacs automatic
fontification, as long as `org-src-fontify-natively' is non-nil."
  (let ((lang-mode (org-src--get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
      (modified (buffer-modified-p))
      (org-buffer (current-buffer))
      (block-faces (let ((face-name (intern (format "org-block-%s" lang))))
         (append (and (facep face-name) (list face-name))
           '(org-block)))))
  (remove-text-properties start end '(face nil))
  (with-current-buffer
      (get-buffer-create
       (format " *org-src-fontification:%s*" lang-mode))
    (erase-buffer)
    (insert string " ") ;; so there's a final property change
    (unless (eq major-mode lang-mode) (funcall lang-mode))
    (org-font-lock-ensure)
    (let ((pos (point-min)) next)
      (while (setq next (next-single-property-change pos 'face))
        (let ((new-face (get-text-property pos 'face)))
    (put-text-property
     (+ start (1- pos)) (1- (+ start next)) 'face
     (list :inherit (append (and new-face (list new-face))
          block-faces))
     org-buffer))
        (setq pos next))
      ;; Add the face to the remaining part of the font.
      (put-text-property (1- (+ start pos))
             end 'face
             (list :inherit block-faces) org-buffer)))
  (add-text-properties
   start end
   '(font-lock-fontified t fontified t font-lock-multiline t))
  (set-buffer-modified-p modified)))))


(defun org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (if (re-search-forward
   "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
   limit t)
  (let ((beg (match-beginning 0))
        (block-start (match-end 0))
        (block-end nil)
        (lang (match-string 7))
        (beg1 (line-beginning-position 2))
        (dc1 (downcase (match-string 2)))
        (dc3 (downcase (match-string 3)))
        end end1 quoting block-type ovl)
    (cond
     ((and (match-end 4) (equal dc3 "+begin"))
      ;; Truly a block
      (setq block-type (downcase (match-string 5))
      quoting (member block-type org-protecting-blocks))
      (when (re-search-forward
       (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
       nil t)  ;; on purpose, we look further than LIMIT
        (setq end (min (point-max) (match-end 0))
        end1 (min (point-max) (1- (match-beginning 0))))
        (setq block-end (match-beginning 0))
        (when quoting
    (org-remove-flyspell-overlays-in beg1 end1)
    (remove-text-properties beg end
          '(display t invisible t intangible t)))
        (add-text-properties
         beg end '(font-lock-fontified t font-lock-multiline t))
        (add-text-properties beg beg1 '(face org-meta-line))
        (org-remove-flyspell-overlays-in beg beg1)
        (add-text-properties  ; For end_src
         end1 (min (point-max) (1+ end)) '(face org-meta-line))
        (org-remove-flyspell-overlays-in end1 end)
        (cond
         ((and lang (not (string= lang "")) org-src-fontify-natively)
    (org-src-font-lock-fontify-block lang block-start block-end)
    (add-text-properties beg1 block-end (list 'src-block t 'lang (substring-no-properties lang))))
         (quoting
    (add-text-properties beg1 (min (point-max) (1+ end1))
             (let ((face-name (intern (format "org-block-%s" lang))))
               (append (and (facep face-name) (list face-name))
                 '(face org-block))))) ; end of source block
         ((not org-fontify-quote-and-verse-blocks))
         ((string= block-type "quote")
    (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
         ((string= block-type "verse")
    (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
        (add-text-properties beg beg1 '(face org-block-begin-line))
        (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
           '(face org-block-end-line))
        t))
     ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
      (org-remove-flyspell-overlays-in
       (match-beginning 0)
       (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
      (add-text-properties
       beg (match-end 3)
       (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
     '(font-lock-fontified t invisible t)
         '(font-lock-fontified t face org-document-info-keyword)))
      (add-text-properties
       (match-beginning 6) (min (point-max) (1+ (match-end 6)))
       (if (string-equal dc1 "+title:")
     '(font-lock-fontified t face org-document-title)
         '(font-lock-fontified t face org-document-info))))
     ((equal dc1 "+caption:")
      (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
      (remove-text-properties (match-beginning 0) (match-end 0)
            '(display t invisible t intangible t))
      (add-text-properties (match-beginning 1) (match-end 3)
         '(font-lock-fontified t face org-meta-line))
      (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
         '(font-lock-fontified t face org-block))
      t)
     ((member dc3 '(" " ""))
      (org-remove-flyspell-overlays-in beg (match-end 0))
      (add-text-properties
       beg (match-end 0)
       '(font-lock-fontified t face font-lock-comment-face)))
     (t ;; just any other in-buffer setting, but not indented
      (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
      (remove-text-properties (match-beginning 0) (match-end 0)
            '(display t invisible t intangible t))
      (add-text-properties beg (match-end 0)
         '(font-lock-fontified t face org-meta-line))
      t))))))


(defface org-block-emacs-lisp
  `((t (:background "LightCyan1")))
  "Face for elisp src blocks")

(defface org-block-python
  `((t (:background "DarkSeaGreen1")))
  "Face for python blocks")

(defface org-block-ipython
  `((t (:background "thistle1")))
  "Face for python blocks")

(defface org-block-jupyter-hy
  `((t (:background "light goldenrod yellow")))
  "Face for hylang blocks")

(defface org-block-sh
  `((t (:background "gray90")))
  "Face for python blocks")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org agenda files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory (expand-file-name "org" personal-dir))
(setq org-agenda-files (list (expand-file-name "home.org" org-directory)
                             (expand-file-name "python.org" org-directory)
                             (expand-file-name "todo.org" org-directory)))

(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)
(setq org-agenda-inhibit-startup t) ;; speedup by ~50x
(setq org-agenda-use-tag-inheritance nil)


;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Display images in org mode enable image mode first                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet compatibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-hook 'org-mode-hook
;;  (lambda ()
;;    (org-set-local 'yas/trigger-key [tab])
;;    (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))
;;
;;(defun yas/org-very-safe-expand ()
;;  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
;;
;;(add-hook 'org-mode-hook
;;  (lambda ()
;;    (make-variable-buffer-local 'yas/trigger-key)
;;    (setq yas/trigger-key [tab])
;;    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
;;    (define-key yas/keymap [tab] 'yas/next-field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag images and files onto org-mode and insert a link to them           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inserting images from web M-x org-easy-img-insert
;;
;; http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/
;; Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode modules for citations, cross-references, bibliographies in the
;; org-mode and useful bibtex tools to go with it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'org-ref)
(require 'org-ref)

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5
      org-ref-bibtex-hydra-key-binding (kbd "H-b"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; allow for export=>beamer by placing
;; http://emacs-fu.blogspot.com/2009/10/writing-presentations-with-org-mode-and.html
;; http://mirror.utexas.edu/ctan/macros/latex/contrib/beamer/doc/beameruserguide.pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
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

;;-----------------------------------------------------------------------------

(provide 'org-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; org-helper-config.el ends here
