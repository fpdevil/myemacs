;;; package  --- bookmarks-config.el --- Bookmark
;;;
;;; Commentary:
;;;
;;; Filename   : bookmarks-config.el
;;; Description: For bookmarks
;;;==========================================================================
(require 'bookmark)

;;;
;;; Code:
;;;


(setq bookmark-sort-flag nil
      bookmark-save-flag 1
      bookmark-default-file (expand-file-name "bookmarks.el" cache-dir)
      bookmark-alist `(("Home"      (filename . "~/"))
                       ("Vim"       (filename . "~/vim"))
                       ("Emacs.d"   (filename . "~emacs.d"))))

(provide 'bookmarks-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; bookmarks-config.el ends here
