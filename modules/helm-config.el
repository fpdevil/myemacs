;;; package --- Helm and related packages for Emacs
;;;
;;; Commentary:
;;; Filename: helm-config.el
;;; description: customizations for helm family settings
;;;
;;; Code:
;;;
(require-package 'helm)

(setq helm-bookmark-show-location t)
(setq helm-buffer-max-length 40)


(after 'helm-source
  (defun /helm/make-source (f &rest args)
    (let ((source-type (cadr args))
          (props (cddr args)))
      (unless (child-of-class-p source-type 'helm-source-async)
        (plist-put props :fuzzy-match t))
      (apply f args)))
  (advice-add 'helm-make-source :around '/helm/make-source))


(after 'helm
  (require-package 'helm-descbinds)


  (require-package 'helm-flx)
  (helm-flx-mode t)


  (require-package 'helm-dash)
  (setq helm-dash-browser-func 'eww)

  (require-package 'helm-describe-modes)      ; Helm interface to Emacs’s describe-mode
  (require 'helm-describe-modes)

  (require-package 'helm-ag)
  (setq helm-ag-fuzzy-match t)
  (after 'helm-ag
    (cond ((executable-find "ag")
           t)
          ((executable-find "pt")
           (setq helm-ag-base-command "pt -e --nogroup --nocolor"))
          ((executable-find "ack")
           (setq helm-ag-base-command "ack --nogroup --nocolor"))))


  (setq helm-swoop-pre-input-function #'ignore)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-use-fuzzy-match t)
  (require-package 'helm-swoop)


  (after "projectile-autoloads"
    (require-package 'helm-projectile))

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; helm interface for company-mode
  (after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:")   'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))

  ;; take between 30 to 50% of the screen space
  (setq helm-autoresize-min-height 30)
  (setq helm-autoresize-max-height 50)

  ;; additional helm variables
  (setq helm-split-window-inside-p                t   ; open helm buffer inside current window, not occupy whole other window
        ;;helm-move-to-line-cycle-in-source       t   ; move to end or start of source when reaching top or bottom of source.
        ;;helm-ff-search-library-in-sexp          t   ; search for library in `require' and `declare-function' sexp.
        ;;helm-ff-file-name-history-use-recentf   t
        ;;helm-scroll-amount                      8   ; scroll 8 lines other window using M-<next>/M-<prior>
        ;;helm-bookmark-show-location             t
        ;;helm-buffer-max-length                  40  ; max length of buffer names before truncate
        ;;helm-prevent-escaping-from-minibuffer   t
        ;;helm-input-idle-delay                   0.1 ; default is 0.01
        ;;helm-display-header-line                t
        ;;helm-reuse-last-window-split-state      t
        ;;helm-split-window-inside-p              t
        helm-org-headings-fontify                 t
        helm-semantic-fuzzy-match                 t
        helm-apropos-fuzzy-match                  t
        helm-mode-fuzzy-match                     t
        helm-imenu-fuzzy-match                    t
        helm-buffers-fuzzy-matching               t
        helm-locate-command                       "mdfind -interpret -name %s %s"
        helm-M-x-fuzzy-match                      t
        ;;helm-completion-in-region-fuzzy-match   t
        ;;helm-ff-skip-boring-files               t
        helm-echo-input-in-header-line            t
        )


  (helm-autoresize-mode t))

(defun /helm/activate-as-switch-engine (on)
  "Switch to the helm navigation system based on the value ON."
  (if on
      (progn
        (global-set-key [remap execute-extended-command] #'helm-M-x)
        (global-set-key [remap find-file] #'helm-find-files)

        ;;------------------------------------------------------------------------------
        ;; global kbd mapping (from prelude emacs - Thanks to Bbatsov)
        ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
        ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
        ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
        ;;------------------------------------------------------------------------------
        (global-set-key (kbd "C-c h")              'helm-command-prefix)
        (global-unset-key (kbd "C-x c"))

        ;; personal key mappings
        (global-set-key (kbd "C-x C-o")            'helm-recentf)              ;; list recently opened files
        (global-set-key (kbd "C-h f" )             'helm-apropos)              ;; apropos helm style
        (global-set-key (kbd "C-c i")              'helm-imenu-in-all-buffers) ;; imenu
        (global-set-key (kbd "C-h C-l")            'helm-locate-library)       ;; locate library
        (global-set-key (kbd "C-h d")              'helm-info-at-point)
        (global-set-key (kbd "C-h i")              'helm-info)
        (global-set-key (kbd "C-h r")              'helm-show-kill-ring)       ;; kill-ring
        (global-set-key (kbd "C-x b")              'helm-mini)                 ;; list current opened buffers
        ;;(global-set-key (kbd "C-x C-b")          'helm-buffers-list)         ;; list of buffers

        ;; add minibuffer history with `helm-minibuffer-history'
        (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
        ;; helm describe modes
        (global-set-key [remap describe-mode] #'helm-describe-modes)
        ;; enable helm mode
        (helm-mode t))
    (global-set-key [remap execute-extended-command] nil)
    (global-set-key [remap find-file] nil)
    (helm-mode -1)))

(when (eq dotemacs-switch-engine 'helm)
  (lazy-init
   (/helm/activate-as-switch-engine t)))

(provide 'helm-config)

;;; helm-config.el ends here.
