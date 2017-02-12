;;; package  --- yasnippets-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : yasnippets-config.el
;;; Description: yasnippet collection(s)
;;;
;;; elisp code for customizing the yasnippets settings
;;;===========================================================================
(require 'yasnippet)                    ;; yasnippet
(require 'helm-c-yasnippet)             ;; helm source for yasnippet
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets configuration                                                 ;;
;; this will install and activate it everywhere.                            ;;
;; your snippets are stored in ~/.emacs.d/snippets.                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas-global-mode 1)
(setq helm-yas-space-match-any-greedy t
      helm-c-yas-display-key-on-candidate t
      yas-wrap-around-region t
      yas-triggers-in-field t)

(setq yas-prompt-functions '(yas-completing-prompt))

(yas-load-directory (concat (getenv "HOME") "/.emacs.d/snippets"))
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))
(add-to-list 'yas-snippet-dirs (concat (getenv "HOME") "/.emacs.d/snippets/"))
(add-to-list 'yas-snippet-dirs (concat (getenv "HOME") "/.emacs.d/snippets/yasnippet-snippets"))

(global-set-key (kbd "C-c y") 'helm-yas-complete)


(provide 'yasnippets-config)

;;; yasnippets-config.el ends here
