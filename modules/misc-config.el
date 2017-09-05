;;; package  --- misc-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : misc-config.el
;;; Description: Miscellaneous configuration and customization for Emacs
;;;              elisp code snippets for customizing Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Code:
;;;

;;----------------------------------------------------------------------------
;; display an initial scratch message & prettify symbols                    ;;
;;----------------------------------------------------------------------------
(setq-default initial-scratch-message
              (concat "ॐ  Emacs With ❤️ " user-login-name "!\n" "☆ సంపత్ కుమార్ ☆" "\n"))

;; symbol prettify
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode +1))


;;----------------------------------------------------------------------------
;; bench marking - check time taken to load each component
;;----------------------------------------------------------------------------
(defun sanityinc/time-subtract-millis (b a)
  "B - A difference multiplied with 1000."
  (* 1000.0 (float-time (time-subtract b a))))

(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around sanityinc/build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (cons feature time)
                       t))))))

(defun sanityinc/show-init-time ()
  "Show Emacs initialization time in milli seconds."
  (message "*** init completed in %.2fms ***"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)

;;----------------------------------------------------------------------------
;; TAB settings - handle whitespaces
;;----------------------------------------------------------------------------
(setq whitespace-style
      '(face tabs empty trailing))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;----------------------------------------------------------------------------
;; check the buffer file name
;;----------------------------------------------------------------------------
(defvar load-user-customized-major-mode-hook t)
(defvar cached-normal-file-full-path nil)

(defun is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
  (let ((f (buffer-file-name))
        org
        (rlt t))
    (cond
     ((not load-user-customized-major-mode-hook) t)
     ((not f)
      ;; file does not exist at all
      (setq rlt t))
     ((string= f cached-normal-file-full-path)
      (setq rlt nil))
     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))
     ((and (string-match "\.html$" f)
           (file-exists-p (setq org (replace-regexp-in-string "\.html$" ".org" f))))
      ;; file is a html file exported from org-mode
      (setq rlt t))
     (t
      (setq cached-normal-file-full-path f)
      (setq rlt nil)))
    rlt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode and YASnippet step on each other toes. These functions are  ;;
;; to help expected TAB function. Attempt these actions, and do the         ;;
;; first one that works.                                                    ;;
;; 1. expand yas snippet                                                    ;;
;; 2. auto complete with company                                            ;;
;; 3. indent                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  "Fallback behaviour for yas."
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "Using TAB for indentation or completion."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; altering the keymaps of company and yas-minor modes
(defun bind-tab-properly ()
  "Binds tab to tab-indent-or-complete, overwritting yas and company bindings."
  (interactive)
  ;;overwrite yas and company tab mappings
  (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete)
  (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map [tab] 'tab-indent-or-complete)
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete))

(add-hook 'company-mode-hook 'bind-tab-properly)

;;----------------------------------------------------------------------------
;; get list of minor modes
;;----------------------------------------------------------------------------
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


;;----------------------------------------------------------------------------
;; add or disable a specific backend in company-backends
;;----------------------------------------------------------------------------
(defun aqua-company-backend-disable (backend mymode)
  "Disable a specific BACKEND in MYMODE in company for a mode."
  (interactive)
  (if (equal major-mode mymode)
      (message "--> disabling %s for %S-hook" backend mymode)
      (when (boundp 'company-backends)
        (make-local-variable 'company-backends)
        ;; disable or remove a backend
        (setq company-backends (delete backend company-backends)))))


(defun aqua-company-backend-add (backend mymode)
  "Add a specific BACKEND in MYMODE in company for a mode."
  (interactive)
  (if (equal major-mode mymode)
      (message "--> adding %s for %S-hook" backend mymode)
      (when (boundp 'company-backends)
        (make-local-variable 'company-backends)
        ;; add a backend
        (add-to-list 'company-backends backend))))


(defun aqua-company-idle-delay (cdelay clength mymode)
  "Set company idle CDELAY, prefix CLENGTH for a specific mode MYMODE."
  (if (equal major-mode mymode)
      (message "--> setting idle delay to %f prefix-length to %d for %S-hook" cdelay clength mymode)
      (setq company-idle-delay cdelay
            company-minimum-prefix-length clength)))

;;----------------------------------------------------------------------------
;; indentation function
;;----------------------------------------------------------------------------
(defun hindent-reformat-buffer-on-save ()
  "Indent an entire buffer with the default indentation scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))


;;----------------------------------------------------------------------------
;; display context sensitive help with eldoc for elisp-mode
;;----------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;;----------------------------------------------------------------------------
;; ibuffer More of the mixed up stuff
;;----------------------------------------------------------------------------
(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("java" (mode . java-mode))
               ("erlang" (mode . erlang-mode))
               ("haskell" (mode . haskell-mode))
               ("javascript" (mode . js-mode))
               ("python" (mode . python-mode))
               ("org" (mode . org-mode))
               ("elisp" (mode . elisp-mode))
               ("xml" (mode . nxml-mode))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-filter-by-filename "."))) ;; to show only dired and files buffers

;;----------------------------------------------------------------------------
;; imenu-list settings (invoke With "-bi")
;;----------------------------------------------------------------------------
(require 'golden-ratio)
(golden-ratio-mode 1)

(require 'imenu-list)
(add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")
(setq imenu-list-focus-after-activation t
      imenu-list-auto-resize t)


;;----------------------------------------------------------------------------
;; show the name of the current function definition in the modeline
;;----------------------------------------------------------------------------
(require 'which-func)
(which-function-mode 1)
;; Show the current function name in the header line
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
;;       ;; We remove Which Function Mode from the mode line,
;;       ;; because it's mostly invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;; remove the ??? when which-func cannot determine name
(setq which-func-unknown "n/a")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; misc-config.el ends here
