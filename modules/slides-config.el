;;; package  --- slides-config.el
;;;
;;; Commentary:
;;;
;;; Filename    : slides-config.el
;;; Description : Create online presentations in Emacs using org-mode
;;;               and the javascript library revealjs.  In case of any
;;;               html5 based slides are needed, we can use ox-html5
;;;
;;; elisp code for customizing the org-mode slideshow settings
;;; https://github.com/hakimel/reveal.js/wiki/Example-Presentations
;;;===========================================================================
(require 'ox-reveal)                       ;; org-reveal
(require 'ox-html5slide)                   ;; org-html5
(require 'org-tree-slide)                  ;; presentations for org
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the location of the reveal js
(setq org-reveal-root (concat "file://" vendor-dir "/javascript/revealjs"))
;;(setq org-reveal-root (expand-file-name (concat vendor-dir "javascript/reveal.js")))
(setq org-reveal-postamble "Sampath Singamsetty")
(setq org-reveal-mathjax t)
(add-hook 'org-mode-hook
          (lambda ()
            (load-library "ox-reveal")))

;; using org-tree-slide
(setq org-tree-slide-skip-outline-level 4)
(org-tree-slide-simple-profile)

(provide 'slides-config)
;;; slides-config.el ends here
