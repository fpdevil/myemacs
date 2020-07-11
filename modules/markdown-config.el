;;; package --- custoom configuration for Markdown Mode
;;;
;;; Commentary:
;;; Filename   : markdown-config.el
;;; description: elisp code for customizing markdown mode for Emacs
;;
;;  fpr rst (Online reStructuredText editor)
;;  http://rst.ninjs.org/
;;
;; examples using pandoc
;; https://github.com/Wandmalfarbe/pandoc-latex-template
;;;
;;; Code:
;;;

(defvar-local pandoc (concat (getenv "HOME") "/.local/bin/pandoc"
                             " -f markdown -t html5 --mathjax --highlight-style espresso -s -c "
                             (expand-file-name "private/nrstyles.css" user-emacs-directory)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command pandoc
              markdown-asymmetric-header t
              markdown-unordered-list-item-prefix "*   "
              markdown-italic-underscore t
              markdown-bold-underscore t
              markdown-reference-location 'end
              markdown-spaces-after-code-fence 0
              markdown-footnote-location 'subtree
              markdown-link-space-sub-char "-"
              markdown-enable-math t
              markdown-coding-system "utf-8"
              markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  )

;; Use visual-line-mode in gfm-mode
(defun my-gfm-mode-hook ()
  "Use visual line mode in GFM-MODE."
  (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)


;;------------------------------------------------------------------------------
;; http server and impatient mode
;;------------------------------------------------------------------------------
(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host "127.0.0.1"))

(use-package impatient-mode
  :ensure t
  :commands impatient-mode)

;; filter function to process the Markdown buffer.
(defun my-markdown-filter (buffer)
  "Filter function to process the Markdown BUFFER."
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))


(defun show-markdown-preview ()
  "Preview the markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))


;; (advice-add 'markdown-preview :around
;;             (lambda (orig &rest args)
;;               "Use Chromium as default browser."
;;               (let ((browse-url-browser-function #'browse-url-firefox))
;;                 (apply orig args))))

;;------------------------------------------------------------------------------
;; org table integration
;;------------------------------------------------------------------------------
(require 'org-table)

(defun markdown-org-table-align-advice ()
  "Replace \"+\" sign with \"|\" in tables."
  (when (member major-mode '(markdown-mode gfm-mode))
    (save-excursion
      (save-restriction
        (narrow-to-region (org-table-begin) (org-table-end))
        (goto-char (point-min))
        (while (search-forward "-+-" nil t)
          (replace-match "-|-"))))))

(advice-add 'org-table-align :after 'markdown-org-table-align-advice)

;;------------------------------------------------------------------------------
;; for mathjax integration
;;------------------------------------------------------------------------------
(setq markdown-xhtml-header-content
      (concat "<script type=\"text/javascript\" async"
              " src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/"
              "2.7.1/MathJax.js?config=TeX-MML-AM_CHTML\">"
              "</script>"))


(provide 'markdown-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; markdown-config.el ends here
