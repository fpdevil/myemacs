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

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets configuration                                                 ;;
;; this will install and activate it everywhere.                            ;;
;; your snippets are stored in ~/.emacs.d/snippets.                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)

(yas-global-mode 1)

;;----------------------------------------------------------------------------
;; yasnippets load directory
;;----------------------------------------------------------------------------
(yas-load-directory (concat (getenv "HOME") "/.emacs.d/snippets"))
(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "/snippets/yasnippet-snippets"))
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))

;; yas verbosity set to trace
(setq yas-verbosity 3)
(yas-reload-all)

;;----------------------------------------------------------------------------
;Add yasnippet support for all company backends
;https://github.com/syl20bnr/spacemacs
;;----------------------------------------------------------------------------
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(setq helm-yas-space-match-any-greedy t
      helm-yas-display-key-on-candidate t
      yas-wrap-around-region t
      yas-triggers-in-field t)

;; setup yasnippet prompt method
(setq yas-prompt-functions '(yas-completing-prompt
                             yas-dropdown-prompt))

;;----------------------------------------------------------------------------
;; smartparens interfering with hippie-expand. disable smartparens temporarily
;; during expansion and revert the same back.
;;----------------------------------------------------------------------------
(defvar aqua--temporarily-enable-smartparens t
  "To keep the state of smartparens to check if its enabled the beginning.")
(defvar aqua--allow-yas-expansions nil
  "To check whether yas expansions are in progress.")

(defun aqua/disable-smartparens-before-snippet-expansion ()
  "This checks `yas-before-expand-snippet-hook'.
Disable smartparens during expansion and keep its initial state."
  (unless aqua--allow-yas-expansions
    (setq aqua--allow-yas-expansions t
          aqua--temporarily-enable-smartparens smartparens-mode))
  (smartparens-mode -1))

(defun aqua/enable-smartparens-after-snippet-expansion ()
  "This checks `yas-after-exit-snippet-hook'.
Get back the initial smartpaens state."
  (setq aqua--allow-yas-expansions nil)
  (when aqua--temporarily-enable-smartparens
    (smartparens-mode 1)))

;;----------------------------------------------------------------------------
;;; yas tab conflict with Org-mode
;; configure Yasnippets for Org Mode
;;----------------------------------------------------------------------------
(defun yas/org-very-safe-expand ()
  "WORKAROUND http://orgmode.org/manual/Conflicts.html yas conflict."
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun yas/org-setup ()
  ;; yasnippet (using the new org-cycle hooks)
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))

;; See https://github.com/eschulte/emacs24-starter-kit/issues/80.
(setq org-src-tab-acts-natively nil)
(add-hook 'org-mode-hook #'yas/org-setup)

;;----------------------------------------------------------------------------
;;; yas cursor customization                                               ;;;
;;; https://github.com/pcmantz/elisp/blob/master/lisp/my-bindings.el       ;;;
;;----------------------------------------------------------------------------
(defvar default-cursor-color "green")
(defvar yasnippet-can-fire-cursor-color "purple")

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  "FIELD Test whether expansion is available and turn green."
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun ss/change-cursor-color-when-can-expand (&optional field)
  "FIELD change the color after expansion."
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (ss/can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun ss/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

; As pointed out by Dmitri, this will make sure it will update color when needed.
(remove-hook 'post-command-hook 'ss/change-cursor-color-when-can-expand)

;;----------------------------------------------------------------------------
;;; yas company conflict resolution
;;; http://emacs.stackexchange.com/questions/7908/how-to-make-yasnippet-and-company-work-nicer
;;----------------------------------------------------------------------------
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand)))
      (if company-candidates
          (company-complete-selection)
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (yas-next-field))))
          (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

;;----------------------------------------------------------------------------
;; loading the helm-c-yasnippet
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c y") 'helm-yas-complete)

(defun aqua/load-yasnippet ()
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun aqua/helm-yasnippet ()
  "Load the helm-c-yasnippet lazily."
  (interactive)
  (require 'helm-c-yasnippet)
  (setq helm-yas-space-match-any-greedy t)
  (call-interactively 'helm-yas-complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to force yasnippets off if required for any mode                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas-force-shutdown ()
  "Force yasnippets down for any mode if needed."
  (interactive)
  (yas-minor-mode -1)
  (setq yas-dont-activate-functions t))

;; to shutdown yasnippets in shell-mode
(add-hook 'shell-mode-hook 'yas-force-shutdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'yasnippets-config)
;;; yasnippets-config.el ends here
