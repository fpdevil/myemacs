;;; package  --- yasnippets-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : yasnippets-config.el
;;; Description: yasnippet collection(s)
;;;
;;; elisp code for customizing the yasnippets settings
;;;
;;; Code:
;;;
;;;

(lazy-init
 (require 'yasnippet)

 ;; be less verbose (trace = 4)
 (setq yas-verbosity 4)
 (setq yas-fallback-behavior 'return-nil)
 (setq yas-indent-line 'auto)
 (setq yas-also-auto-indent-first-line t)
 (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt yas/dropdown-prompt))
 (yas-load-directory (concat user-emacs-directory "snippets"))
 ;; personal snippets
 ;; (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))

 (yas-reload-all)
 ;; (yas-global-mode +1)
 (add-hook 'prog-mode-hook 'yas-minor-mode)
 (dolist (hook
          (list
           'c-mode-common-hook
           'js2-mode-hook
           'org-mode-hook
           'python-mode-hook
           'emacs-lisp-mode-hook
           'html-mode-hook))
   (add-hook hook #'yas-minor-mode))

 ;; Disable yasnippet for specific modes.
 ;; (progn
 ;;     (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))
 ;;** to shutdown yasnippets in shell-mode
 (add-hook 'shell-mode-hook 'yas-force-shutdown)

 ;;** to force yasnippets off if required for any mode
 (defun yas-force-shutdown ()
   "Force yasnippets down for any mode if needed."
   (interactive)
   (yas-minor-mode -1)
   (setq yas-dont-activate-functions t)))

(provide 'yas-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; yasnippets-config.el ends here
