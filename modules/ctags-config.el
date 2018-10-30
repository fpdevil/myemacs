;;; package --- ctags-config.el for ctags generation
;;;
;;; Commentary:
;;;
;;; Filename   : ctags-config.el
;;; Description: CTags generation helper
;;;              (@ref https://github.com/redguardtoo/emacs.d)
;;;
;;; Code:
;;;
;;=============================================================================
(lazy-init
 ;; Don't ask before rereading the TAGS files if they have changed
 (setq tags-revert-without-query t)
 ;; Do case-sensitive tag searches
 (setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
 ;; Don't warn when TAGS files are large
 (setq large-file-warning-threshold nil)

 (if (aqua/is-mac)
     ;; Mac's default ctags does not support -e option
     ;; If you install Emacs by homebrew, another version of etags is already
     ;; installed which does not need flag -e too
     ;; the best option is to install latest ctags from sf.net
     (setq ctags-command "/usr/local/bin/ctags -e -R "))
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;; ctags config ends here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ctags-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ctags-config.el ends here
