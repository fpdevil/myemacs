;;; package  --- slides-config.el
;;;
;;; Commentary:
;;;
;;; Filename    : slides-config.el
;;; Description : Create online presentations in Emacs using org-mode
;;;               and the javascript library revealjs.  In case of any
;;;               html5 based slides are needed, we can use ox-html5
;;;
;; elisp code for customizing the org-mode slideshow settings
;; https://github.com/hakimel/reveal.js/wiki/Example-Presentations
;; https://github.com/jr0cket/slides/tree/gh-pages
;;
;; #+OPTIONS: num:nil toc:nil
;; #+REVEAL_TRANS: None/Fade/Slide/Convex/Concave/Zoom
;; #+REVEAL_THEME: Black/White/League/Sky/Beige/Simple/Serif/Blood/Night/Moon/Solarized
;; #+Title: Title of Your Talk
;; #+Author: Your Name
;; #+Email: Your Email Address or Twitter Handle
;;
;;;
;;; Code:
;;;

(require 'ox-reveal)                       ;; org-reveal
(require 'ox-html5slide)                   ;; org-html5
(require 'org-tree-slide)                  ;; presentations for org
(require 'htmlize)

;;**  reveal.js setup
;;    set the location of the reveal js
;;(setq org-reveal-root (expand-file-name (concat vendor-dir "javascript/reveal.js")))
(setq org-reveal-root (concat "file:///" vendor-dir "/javascript/reveal.js"))
(setq org-reveal-postamble "Sampath Singamsetty")
(setq org-reveal-hlevel 1)
(setq org-reveal-theme "simple") ;beige blood moon night serif simple sky solarized
(setq org-reveal-mathjax t)      ;use mathjax.org to render LaTeX equations
(add-hook 'org-mode-hook
          (lambda ()
            (load-library "ox-reveal")))

;; using org-tree-slide
(setq org-tree-slide-skip-outline-level 4)
(org-tree-slide-simple-profile)

;; to fix conflict with rainbow-delimiters
(defadvice htmlize-buffer-1 (around ome-htmlize-buffer-1 disable)
  (rainbow-delimiters-mode -1)
  ad-do-it
  (rainbow-delimiters-mode t))

(defun ome-htmlize-setup ()
  (if (el-get-read-package-status 'rainbow-delimiters)
      (progn
        (ad-enable-advice 'htmlize-buffer-1 'around 'ome-htmlize-buffer-1)
        (ad-activate 'htmlize-buffer-1))))


(provide 'slides-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; slides-config.el ends here
