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
;;==============================================================================
(require 'ac-cider)                            ;; clojure completion source
(require 'clojure-mode-extra-font-locking)     ;; clojure syntax highlighting

;;-----------------------------------------------------------------------------
;; CLOJURE SETUP
;;-----------------------------------------------------------------------------
(add-hook 'clojure-mode-hook 'enable-paredit-mode)   ;; paredit
(add-hook 'clojure-mode-hook 'subword-mode)          ;; for working with java classes
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)    ;; documentation

;;-----------------------------------------------------------------------------
;; syntax hilighting and indentation
;;-----------------------------------------------------------------------------
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
            (enable-paredit-mode)
            (subword-mode)))


;;-----------------------------------------------------------------------------
;; for cider
;;-----------------------------------------------------------------------------
(add-hook 'clojure-mode-hook 'cider-mode)    ;; enter cider mode when entering the clojure
(add-hook 'cider-mode-hook #'eldoc-mode)     ;; provides minibuffer documentation
(setq cider-repl-pop-to-buffer-on-connect t
      cider-repl-display-help-banner nil
      cider-show-error-buffer nil
      cider-auto-select-error-buffer t
      cider-repl-history-file "~/.emacs.d/cache/cider-history"
      cider-repl-wrap-history t
      nrepl-log-messages t
      nrepl-popup-stacktraces nil
      cider-font-lock-dynamically '(macro core function var)
      nrepl-hide-special-buffers t
      cider-overlays-use-font-lock t
      cider-repl-use-pretty-printing t)


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
;; helm, paredit and smartparens integration
;;-----------------------------------------------------------------------------
(after-load 'cider-mode
  (add-hook 'cider-mode-hook #'helm-cider-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))
;;(add-hook 'cider-repl-mode-hook #'paredit-mode)            ;; enable paredit in repl
;;(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)


;;-----------------------------------------------------------------------------
;; clojure mode for other extension
;;-----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '(".cljs.hl$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

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
;; cider auto completion
;;-----------------------------------------------------------------------------
(defun set-auto-complete-as-completion-at-point-function ()
  "Auto completion at a point from minibuffer.el."
  (setq completion-at-point-functions '(auto-complete)))

(after-load 'auto-complete
  (require-package 'ac-nrepl)
  '(progn
     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
     (add-hook 'cider-mode-hook 'ac-cider-setup)
     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
     (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
     (add-hook 'cider-mode-hook 'ac-nrepl-setup)
     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(defun clj-align-vectors (beg end)
  "Aligh clojure vectors BEG to END."
  (interactive "r")
  (align-regexp beg end "^ \\[[^ ]+\\(\\s-+\\)" 1 1 t))

;; with company
(after-load 'company
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))


;;-----------------------------------------------------------------------------
;; Look up Clojure documentation in a pop-up with CIDER’s functionality
;;-----------------------------------------------------------------------------
(defun get-cider-symbol-full-doc (symbol)
  "Return a string of full documentation of SYMBOL, as given by `cider-create-doc-buffer'."
  (let ((buf (cider-create-doc-buffer symbol)))
    (when buf
      (with-current-buffer buf
        (buffer-substring (point-min)
                          ;; `-10' to exclude "[source]" line
                          (- (point-max) 10))))))

(defun get-cider-doc-popup ()
  "Display CIDER documentation in a popup."
  (interactive)
  (funcall (quick-peek-make-doc-command #'get-cider-symbol-full-doc #'cider-symbol-at-point)))

(defun get-cider-doc-popup-on ()
  "Turn `get-cider-doc-popup' by binding it to an appropriate key."
  (define-key cider-mode-map (kbd "C-h C-j") #'get-cider-doc-popup))

;; Only use pop-up documentation when CIDER is connected
(add-hook 'cider-connected-hook #'get-cider-doc-popup-on)

;;-----------------------------------------------------------------------------
;; clj-refactor and dependencies
;;-----------------------------------------------------------------------------
(require-package 'clj-refactor)

(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            ;; insert keybinding setup here
            (cljr-add-keybindings-with-prefix "C-c RET")))

(add-hook 'clojure-mode-hook #'yas-minor-mode)
(setq cljr-auto-sort-ns nil)                 ; no auto sort
(setq cljr-favor-prefix-notation nil)        ; do not prefer prefixes when using clean-ns


;;-----------------------------------------------------------------------------
;; flycheck setup
;;-----------------------------------------------------------------------------
(require-package 'let-alist)
(require-package 'flycheck-clojure)
(after-load 'clojure-mode
  (after-load 'cider
    (after-load 'flycheck
      (flycheck-clojure-setup))))

;;-----------------------------------------------------------------------------

(provide 'clojure-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; clojure-config.el ends here
