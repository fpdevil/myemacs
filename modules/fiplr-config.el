;;; fiplr-config.el --- Fuzzy finder for files in a project.
;;;
;;; Commentary:
;;;
;;; Filename   : fiplr-config.el
;;; Description: Emacs Fuzzy Finder settings
;;;              https://github.com/grizzl/fiplr
;;;
;;===========================================================================
(require 'fiplr)

;;; Code:

(with-eval-after-load 'fiplr
  (setq fiplr-root-markers '(".git" ".svn"))
  (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                              (files ("*.jpg" "*.png" "*.zip" "*~"))))
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

(provide 'fiplr-config)

;;; fiplr-config.el ends here
