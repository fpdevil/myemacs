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
;;;===========================================================================

(lazy-init

 ;; install yasnippet package if not available
 (require-package 'yasnippet)
 ;; -- load the yasnippet library
 (require 'yasnippet)

 (setq yas/trigger-key "<C-tab>") ;; make sure this is before yas/initialize

 ;; -- use C-Tab for snippet expansion
 (define-key yas-minor-mode-map (kbd "<tab>") nil)
 (define-key yas-minor-mode-map (kbd "TAB") nil)
 (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
 (define-key global-map (kbd "C-c o") 'iedit-mode)

 ;; -- loading the helm-c-yasnippet
 (global-set-key (kbd "C-c y") 'helm-yas-complete)

 ;; -- yasnippets configuration
 ;; this will install and activate it everywhere, snippets are stored in ~/.emacs.d/snippets
 ;; (yas/initialize)

 ;; -- function for loading a snippet
 (defun aqua/load-yasnippet ()
   (unless yas-global-mode (yas-global-mode 1))
   (yas-minor-mode 1))

 ;; -- enable the yas' specific settings
 (setq yas-verbosity 4)                       ;; be less verbose (trace = 4)
 (setq yas-indent-line 'auto)
 (setq yas-also-auto-indent-first-line t)

 (yas-global-mode 1)


 ;; snippets collection
 (require-package 'yasnippet-snippets)


 ;; -- loding the helm snippet lazily
 (defun aqua/helm-yasnippet ()
   "Load the helm-c-yasnippet lazily."
   (interactive)
   (require 'helm-c-yasnippet)
   (setq helm-yas-space-match-any-greedy t
         helm-yas-display-key-on-candidate t
         yas-wrap-around-region t
         yas-triggers-in-field t)
   (call-interactively 'helm-yas-complete))


 ;; -- handling the prompt with helm
 (defun yas-helm-prompt (prompt choices &optional display-fn)
   "Use helm to select a snippet."
   (interactive)
   (setq display-fn (or display-fn 'identity))
   (if (require 'helm-config)
       (let (tmpsource cands result rmap)
         (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
         (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
         (setq tmpsource
               (list
                (cons 'name prompt)
                (cons 'candidates cands)
                '(action . (("Expand" . (lambda (selection) selection))))
                ))
         (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
         (if (null result)
             (signal 'quit "user quit!")
           (cdr (assoc result rmap))))
     nil))


 ;; setup yasnippet prompt method
 ;; The yas-prompt-functions variable is only consulted if you expand from a snippet
 ;; key and there are multiple snippets with the same key, or if you use yas-insert-
 ;; snippet. If you are using autocomplete to expand snippets, it has no effect
 (setq yas-prompt-functions '(yas-helm-prompt
                              yas-ido-prompt
                              yas-completing-prompt
                              yas-dropdown-prompt))

 (yas-reload-all)

 ;; -- yasnippets load directory
 ;;(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
 ;;(yas-load-directory (expand-file-name "snippets" user-emacs-directory))
 (add-hook 'term-mode-hook
           (lambda()
             (setq yas-dont-activate-functions t)))

 ;; -- add the yas hook
 (defun yasnippet-generic-setup-for-mode-hook ()
   "Enable yas minor mode."
   (unless (aqua/is-buffer-file-temp)
     (yas-minor-mode 1)))

 ;; (add-hook 'prog-mode-hook 'yasnippet-generic-setup-for-mode-hook)
 ;; (add-hook 'html-mode-hook 'yasnippet-generic-setup-for-mode-hook)
 ;; (add-hook 'text-mode-hook 'yasnippet-generic-setup-for-mode-hook)
 ;; ;; below modes does NOT inherit from prog-mode
 ;; (add-hook 'cmake-mode-hook 'yasnippet-generic-setup-for-mode-hook)
 ;; (add-hook 'web-mode-hook 'yasnippet-generic-setup-for-mode-hook)
 ;; (add-hook 'scss-mode-hook 'yasnippet-generic-setup-for-mode-hook)


 ;; use yas-completing-prompt when ONLY when `M-x yas-insert-snippet'
 (defadvice yas-insert-snippet (around use-completing-prompt activate)
   "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
   (let* ((yas-prompt-functions '(yas-completing-prompt)))
     ad-do-it))

 ;;----------------------------------------------------------------------------
  ;;; to force yasnippets off if required for any mode
 ;;----------------------------------------------------------------------------
 (defun yas-force-shutdown ()
   "Force yasnippets down for any mode if needed."
   (interactive)
   (yas-minor-mode -1)
   (setq yas-dont-activate-functions t))

 ;; to shutdown yasnippets in shell-mode
 (add-hook 'shell-mode-hook 'yas-force-shutdown)
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'yasnippets-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; yasnippets-config.el ends here
