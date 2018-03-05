;;; package --- org mode configuration
;;;
;;; Commentary:
;;;
;;; Filename   : org-config.el
;;; Description: Show bullets in org-mode as UTF-8 characters
;;;              configuration file for org bullets mode
;;;              https://github.com/sabof/org-bullets
;;;              follow: http://doc.norang.ca/org-mode.html
;;;
;;; shell command execution example (C-c C-c) also include in export
;;; #+begin_src sh :results output :exports both
;;;   df -Ph
;;; #+end_src
;;;
;;; elisp code for org mode configuration support and handling
;;;
;;; Code:
;;;
;;;============================================================================
(require 'org)
(require 'org-install)
(require 'org-bullets)
(require 'org-ac)
(require 'org-easy-img-insert)
(require 'ox-latex)
(require 'ox-html)
(require 'ox-reveal)
(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-org)
(require 'ox-ascii)


;; -- basic variable setup
;; == file mode association
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; == do not use auto-fill in org-mode
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; == allow lists with letters in them
(setq org-list-allow-alphabetical t)

;; -- miscellaneous settings for org-mode
;; == renumber the footnotes when new footnotes are inserted
(setq org-footnote-auto-adjust t)

;; == default with the images open
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  turn on visual-line-mode for Org-mode only                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  some custom settings and todo configuration from pragmatic emacs       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-list-description-max-indent 5)    ;; set max indentation for description lines
(setq org-adapt-indentation nil)            ;; prevent demoting heading, shifting text in sections

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  for src code syntax highlighting during export                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-latex-listings t)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "parskip"))


(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  pdf export options                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  auto complete sources for org                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-ac/config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  org bullets for markdown                                                 ;;
;;  use org-bullets-mode for utf8 symbols as org bullets                     ;;
;;  select, do [M-x eval-region]. The *s will be replaced with utf-8 bullets ;;
;;  next time you open an org file                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("●" "○" "◆" "◇" "▸"))
(add-hook 'org-mode-hook #'org-bullets-mode)


(setq org-todo-keywords
      '((sequence "☛ TODO(t)" "₪ NEXT(n@)" "|" "✔ DONE(d@)")
        (sequence "⚑ WAITING(w@/!)" "⟁ HOLD(h@/!)" "|")
        (sequence "|" "✘ CANCELED(c@/!)" "|" "§ POSTPONED(p@/!)" "PHONE" "MEETING")))

;; change the elipsis face
;; (setq org-ellipsis "⚡⚡⚡")

;; fast TODO selection
(setq org-use-fast-todo-selection t)
;; change a state using C-c C-t KEY where KEY is one of org-todo-keywords
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  inserting code blocks                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   making ispell work with the org-mode                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'org-ispell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   org babel loads                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'org
  (lambda()
    (require 'ob-emacs-lisp)
    (require 'ob-http)
    (require 'ob-latex)
    (require 'ob-haskell)
    (require 'ob-scala)
    (require 'ob-python)
    (require 'ob-C)
    (require 'ob-clojure)
    (require 'ob-shell)
    (require 'ob-org)
    (require 'ob-awk)
    (require 'ob-sed)
    (require 'ob-css)
    (require 'ob-js)
    (setq org-export-babel-evaluate nil)                                ;; do not export code on export by default
    (setq org-startup-indented t)
    (setq org-imenu-depth 3)                                            ;; increase imenu depth to include third level headings
    ;; Set sensible mode for editing dot files
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images) ;; update images from babel code blocks automatically

  ;; eye candies and font coloring in code blocks
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)                                  ;; have completion in blocks
    (setq org-confirm-babel-evaluate nil)                               ;; coding without prompt
    (setq org-hide-leading-stars t)
    (setq org-alphabetical-lists t)
    (setq org-hide-emphasis-markers t)                                  ;; hide the *,=, or / markers
    (setq org-pretty-entities t)                                        ;; have \alpha, \to and others display as utf8
    (setq org-confirm-elisp-link-function nil)
    (setq org-confirm-shell-link-function nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove key bindings for some motions                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;; Load Helper and utilities configuration                               ;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (concat module-dir "/org-helper-config.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  use imagemagick to preview latex                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq org-latex-create-formula-image-program 'imagemagick)
(setq org-preview-latex-default-process 'imagemagick)

;;-----------------------------------------------------------------------------
;; use pre-evaluated results directly and avoid repeated evaluation again
;; during the insertion of src templates in pdf export
;;-----------------------------------------------------------------------------
(setq org-export-babel-evaluate 'inline-only)

;;-----------------------------------------------------------------------------
;; Prettier (or at least fancier) code block delimiters
;;-----------------------------------------------------------------------------
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; org-config.el ends here
