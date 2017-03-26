;;; package --- clojure-config.el configuration settings for clojure
;;;
;;; Commentary:
;;;
;;; Filename   : clojure-config.el
;;; Description: Clojure configuration and customization using the
;;;              relevant plugins and lein.
;;;
;;===========================================================================
(require 'ac-cider)                                       ;; clojure completion source
(require 'clojure-mode-extra-font-locking)                ;; clojure syntax highlighting
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE SETUP                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'clojure-mode-hook 'enable-paredit-mode)        ;; paredit
(add-hook 'clojure-mode-hook 'subword-mode)               ;; for working with java classes
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)         ;; documentation

;; syntax hilighting for clj
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (setq inferior-lisp-program "lein repl")
;;             (font-lock-add-keywords
;;              nil
;;              '(("(\\(facts?\\)"
;;                 (1 font-lock-keyword-face))
;;                ("(\\(background?\\)"
;;                 (1 font-lock-keyword-face))))
;;             (define-clojure-indent (fact 1))
;;             (define-clojure-indent (facts 1))))




;; syntax hilighting and indentation
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


;;
; for cider
;;
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)     ;; provides minibuffer documentation
(setq cider-repl-pop-to-buffer-on-connect t
      cider-show-error-buffer t
      cider-auto-select-error-buffer t
      cider-repl-history-file (concat (getenv "HOME") "/.emacs.d/cache/cider-history")
      cider-repl-wrap-history t
      nrepl-log-messages t
      cider-font-lock-dynamically '(macro core function var)
      nrepl-hide-special-buffers t
      cider-overlays-use-font-lock t)

;; helm
(helm-cider-mode 1)

(add-hook 'cider-repl-mode-hook 'paredit-mode)            ;; enable paredit in repl

;;
; clojure mode for other extensions
;;
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;;
; shortcuts
;;
(defun cider-start-http-server ()
  "Start a HTTP Server."
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  "Refresh the cider repl."
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

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


;;
; cider auto completion
;;
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))


(provide 'clojure-config)

;;; clojure-config.el ends here
