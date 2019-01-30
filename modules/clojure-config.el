;;; package --- clojure-config.el configuration settings for clojure
;;;
;;; Commentary:
;;;
;;; Filename   : clojure-config.el
;;; Description: Clojure configuration and customization using the
;;;              relevant plugins and lein.
;;;
;;;
;;; Code:
;;;



(require-package 'clojure-mode)
(require-package 'cider)


;;-----------------------------------------------------------------------------
;;** fancy symbols for clojure
;;-----------------------------------------------------------------------------
(require 'clojure-mode-extra-font-locking)     ;; clojure syntax highlighting
(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and set's
like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6} for MODE."
  (font-lock-add-keywords mode
                          `(("(\\(fn\\)[\[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "λ"))))
                            ("(\\(partial\\)[\[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "Ƥ"))))
                            ("(\\(comp\\)[\[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∘"))))
                            ("\\(#\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ"))))
                            ("\\(#\\){"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∈")))))))

(dolist (c '(clojure-mode cider-repl-mode cider-clojure-interaction-mode))
  (clojure/fancify-symbols c))


;;-----------------------------------------------------------------------------
;;** great lisp coding hooks for clojure
;;-----------------------------------------------------------------------------
(defun clj-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq clj-coding-hook 'clj-coding-defaults)

(eval-after-load 'clojure-mode
  '(progn
     (defun clj-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'clj-coding-hook))

     (setq clj-mode-hook 'clj-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'clj-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t
           cider-font-lock-dynamically '(macro core function var)
           cider-overlays-use-font-lock t
           cider-repl-use-pretty-printing t
           cider-macroexpansion-print-metadata t
           cider-repl-display-help-banner t
           nrepl-buffer-name-show-port t
           cider-completion-annotations-include-ns 'always
           cider-prompt-for-symbol nil
           cider-repl-history-file (concat cache-dir "/cider-history")
           )

     (add-hook 'cider-mode-hook 'eldoc-mode)
     (add-hook 'cider-repl-mode-hook 'eldoc-mode)

     (defun clj-cider-repl-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'clj-coding-hook))

     (setq clj-cider-repl-mode-hook 'clj-cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'clj-cider-repl-mode-hook)))))



;; helm integration
(after 'helm
  (add-hook 'cider-mode-hook #'helm-cider-mode))

(after 'evil
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (evil-set-initial-state 'cider-browse-ns-mode 'motion)
  (evil-set-initial-state 'cider-stacktrace-mode 'motion)
  (evil-set-initial-state 'cider-repl-mode 'emacs))

;;-----------------------------------------------------------------------------
;; syntax highlighting and indentation
;;-----------------------------------------------------------------------------
(setq clojure-indent-style :always-align)
(setq clojure-indent-style :always-indent)
(setq clojure-indent-style :align-arguments)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (put-clojure-indent 'reg-event-db 1)
            (put-clojure-indent 'reg-event-fx 1)
            (put-clojure-indent 'reg-fx 1)
            (put-clojure-indent 'reg-cofx 1)
            (put-clojure-indent 'reg-sub 1)
            ))

;;** aggressive indentation for clojure
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojurescript-mode-hook #'aggressive-indent-mode)

;;** eldoc and cider
(define-advice cider-eldoc-format-function (:around (old-fun thing pos eldoc-info) docstring)
  "Show docstring for function as well."
  (concat
   (funcall old-fun thing pos eldoc-info)
   (when-let* ((doc (lax-plist-get eldoc-info "docstring"))
               (doc-one-line (substring doc 0 (string-match "\n" doc))))
     (concat "  |  " (propertize doc-one-line 'face 'italic)))))

(defun cider-reset ()
  "Reset the CIDER repl."
  (interactive)
  (cider-interactive-eval "(user/reset)"))

(defun cider-refresh ()
  "Refresh the CIDER repl."
  (interactive)
  (cider-interactive-eval "(clojure.tools.namespace.repl/refresh)"))

;;-----------------------------------------------------------------------------
;; shortcuts
;;-----------------------------------------------------------------------------
(defun cider-start-http-server ()
  "Start a HTTP Server."
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-user-ns ()
  "Take care of the clojure name space."
  (interactive)
  (cider-repl-set-ns "boot.user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))


;;-----------------------------------------------------------------------------
;;** [Company] - auto completion
;;-----------------------------------------------------------------------------
(after 'company
  (setq company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  ;; (add-hook 'clojure-mode-hook
  ;;           (lambda () (company-config/push-company-backends-locally 'company-dabbrev)))
  ;; (add-hook 'cider-repl-mode-hook
  ;;           (lambda () (company-config/push-company-backends-locally 'company-dabbrev)))

  )


;;-----------------------------------------------------------------------------
;; [AC] - cider auto completion
;;-----------------------------------------------------------------------------
(defun set-auto-complete-as-completion-at-point-function ()
  "Auto completion at a point from minibuffer.el."
  (setq completion-at-point-functions '(auto-complete)))

(defun clj-ac-complete ()
  "Auto complete bootstrapping for clojure."
  (add-hook 'cider-repl-mode-hook (lambda () (auto-complete-mode 1)))
  ;;
  ;; ac-nrepl is deprecated in favor of ac-cider
  ;;(require-package 'ac-nrepl)
  ;;(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
  ;;(add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (require-package 'ac-cider)   ;; clojure completion source
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode))
  )


(when (eq dotemacs-completion-engine 'auto-complete)
  ;; enable ac for repl
  (add-hook 'cider-repl-mode-hook (lambda () (auto-complete-mode 1)))
  (clj-ac-complete))


;;-----------------------------------------------------------------------------
;; clj-refactor and dependencies
;;-----------------------------------------------------------------------------
(defun clojure-refactoring ()
  "Enable Clojure refactoring support."
  (require-package 'clj-refactor)
  ;; Add clj-refactor to clojure-mode
  (add-hook 'clojure-mode-hook '(lambda () (clj-refactor-mode 1)))
  (setq cljr-auto-sort-ns nil                 ; no auto sort
        cljr-favor-prefix-notation nil)       ; do not prefer prefixes when using clean-ns
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; (clojure-refacting)

;;-----------------------------------------------------------------------------
;;** [monroe] - a better repl for clojure | M-x monroe [RET]
;;-----------------------------------------------------------------------------
(require-package 'monroe)
(require 'monroe)
(autoload 'clojure-enable-monroe "Starts monroe." t)
(add-hook 'clojure-mode-hook 'clojure-enable-monroe)


;; golden ratio support
(with-eval-after-load 'golden-ratio
  (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))


;;-----------------------------------------------------------------------------
;; FlyCheck setup
;;-----------------------------------------------------------------------------
(require-package 'let-alist)
(require-package 'flycheck-clojure)
(after-load 'flycheck
  (flycheck-clojure-setup))



(provide 'clojure-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; clojure-config.el ends here
