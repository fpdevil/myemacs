;;; package  --- helm-settings-config.el
;;;
;;; Commentary:
;;;
;;; Filename: helm-settings-config.el
;;; Description: Emacs incremental completion and selection narrowing framework
;;;              configuration file for HELM settings.
;;;              A major/minor mode for helm based help utilities
;;;
;;; elisp code for customizing the helm settings
;;;
;;; Code:
;;;
;;;=============================================================================
(require 'helm)
(require 'helm-config)

;;------------------------------------------------------------------------------
;;; custom functions
;;------------------------------------------------------------------------------
(defun helm-hide-minibuffer-maybe ()
  "To hide the text you type in the mini buffer after pressing `M-x' or `C-x' etc in Helm."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun set-helm-dotted-directory ()
  "Set the face of directories for `.' and `..'."
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground nil
                      :background nil
                      :inherit 'helm-ff-directory))

;;------------------------------------------------------------------------------
;;; Map pairs of simultaneously pressed keys to commands
;;------------------------------------------------------------------------------
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
(key-chord-define-global "fm" 'helm-mini)

;;------------------------------------------------------------------------------
;;; helm set custom variables
;;------------------------------------------------------------------------------
(setq helm-bookmark-show-location t)
(setq helm-buffer-max-length 40)

;; -- for helm sources
(after 'helm-source
  (defun my-helm-make-source (f &rest args)
    (let ((source-type (cadr args))
          (props (cddr args)))
      (unless (child-of-class-p source-type 'helm-source-async)
        (plist-put props :fuzzy-match t))
      (apply f args)))
  (advice-add 'helm-make-source :around 'my-helm-make-source))

(after 'helm
  ;; uncomment to hide the text from mini buffer
  ;;(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (require-package 'helm-descbinds)
  (helm-descbinds-mode 1)
  (global-set-key (kbd "C-c h h") 'helm-descbinds)
  (setq helm-descbinds-window-style 'split-window)

  (require-package 'helm-flx)
  (helm-flx-mode t)
  (setq helm-flx-for-helm-find-files t ;; t by default
        helm-flx-for-helm-locate t) ;; nil by default

  (require-package 'helm-fuzzier)
  (helm-fuzzier-mode t)

  (require-package 'helm-dash)
  (setq helm-dash-browser-func 'eww)
  (defun c-doc ()
    (setq helm-dash-docsets '("C")))
  (defun c++-doc ()
    (setq helm-dash-docsets '("C" "C++")))
  (add-hook 'c-mode-hook 'c-doc)
  (add-hook 'c++-mode-hook 'c++-doc)

  ;; helm silver search
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

  (add-hook 'helm-find-files-before-init-hook 'set-helm-dotted-directory)

  ;; set helm variables
  (setq helm-split-window-in-side-p             t   ; open helm buffer inside current window, not occupy whole other window
        ;; helm-move-to-line-cycle-in-source    nil ; move to end or beginning of source when reaching top or bottom of source.
        ;; helm-ff-search-library-in-sexp       t   ; search for library in `require' and `declare-function' sexp.
        ;; helm-scroll-amount                   8   ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-input-idle-delay                   0.1 ; default is 0.01
        helm-ff-file-name-history-use-recentf   t
        helm-prevent-escaping-from-minibuffer   t
        helm-bookmark-show-location             t
        helm-display-header-line                t
        ;; percentage of size 10% to 80% of screen space
        helm-autoresize-min-height              10
        helm-autoresize-max-height              80
        helm-org-headings-fontify               t
        helm-semantic-fuzzy-match               t
        helm-apropos-fuzzy-match                t
        helm-imenu-fuzzy-match                  t
        helm-buffers-fuzzy-matching             t
        helm-locate-command                     "mdfind -interpret -name %s %s"
        helm-M-x-fuzzy-match                    t
        helm-completion-in-region-fuzzy-match   t
        helm-echo-input-in-header-line          t)

  (helm-autoresize-mode t))

;;------------------------------------------------------------------------------
;; global kbd mapping (from prelude emacs - Thanks to Bbatsov)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;------------------------------------------------------------------------------
(global-set-key (kbd "C-c h")              'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap find-file] #'helm-find-files)

;; personal mappings
;;(global-set-key (kbd "C-x C-f")          'helm-find-files)           ;; list files
;;(global-set-key (kbd "M-x")              'helm-M-x)
(global-set-key (kbd "C-x C-o")            'helm-recentf)              ;; list recently opened files
(global-set-key (kbd "C-x b")              'helm-mini)                 ;; list current opened buffers
(global-set-key (kbd "C-x C-b")            'helm-buffers-list)         ;; list of buffers
(global-set-key (kbd "C-h f" )             'helm-apropos)              ;; apropos helm style
(global-set-key (kbd "C-c i")              'helm-imenu-in-all-buffers) ;; imenu
(global-set-key (kbd "M-y")                'helm-show-kill-ring)       ;; kill-ring
(global-set-key (kbd "C-h r")              'helm-info-emacs)           ;; Emacs info
(global-set-key (kbd "C-h C-l")            'helm-locate-library)       ;; locate library
(global-set-key (kbd "C-h d")              'helm-info-at-point)
(global-set-key (kbd "C-h i")              'helm-info)
;;(key-chord-define-global "fm"   'helm-mini)

;; add minibuffer history with `helm-minibuffer-history'
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(require 'helm-describe-modes)      ; Helm interface to Emacs’s describe-mode
(global-set-key [remap describe-mode] #'helm-describe-modes)

;; using helm for listing the eshell history
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history
                                         'helm-eshell-history eshell-mode-map)))

;; helm theme selector
(require-package 'helm-themes)
(require 'helm-themes)

;; helm interface for company-mode
(after 'company
  '(progn
     (define-key company-mode-map (kbd "C-:")   'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; enable modes
(helm-mode t)
(helm-popup-tip-mode 1)

;;------------------------------------------------------------------------------

(provide 'helm-settings-config)
;;; helm-settings-config.el ends here
