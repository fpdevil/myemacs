;;; package  --- yasnippets-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : yasnippets-config.el
;;; Description: yasnippet collection(s)
;;;
;;; elisp code for customizing the yasnippets settings
;;;==========================================================================
(require 'yasnippet)                    ;; yasnippet
(require 'helm-c-yasnippet)             ;; helm source for yasnippet
;
;;; Code:
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets configuration                                                 ;;
;; this will install and activate it everywhere.                            ;;
;; your snippets are stored in ~/.emacs.d/snippets.                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas-global-mode 1)
(setq helm-yas-space-match-any-greedy t)
(yas-load-directory (concat (getenv "HOME") "/.emacs.d/snippets"))
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))
(add-to-list 'yas-snippet-dirs (concat (getenv "HOME") "/.emacs.d/snippets/"))
(add-to-list 'yas-snippet-dirs (concat (getenv "HOME") "/.emacs.d/snippets/yasnippet-snippets"))


(provide 'yasnippets-config)

;;; yasnippets-config.el ends here