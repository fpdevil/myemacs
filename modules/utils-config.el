;;; package  --- utils-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : utils-config.el
;;; Description: Some utilities and functions which does not have a proper
;;;              place or location
;;;              Using some functions from the excellent purcell Emacs.
;;; elisp code for customizing multiple things
;;;
;;; Code:
;;;

;;----------------------------------------------------------------------------
;;* all utilities section
;;----------------------------------------------------------------------------
(lazy-init
 ;;**
 ;; window switching, the visual way
 ;; *visual* way to choose a window to switch (visual replacement for -> C-x o)
 ;;(require-package 'switch-window)
 ;;(require 'switch-window)

 (require-package 'pos-tip)
 (require 'pos-tip)
 (setq pos-tip-background-color "#FFFACE"
       pos-tip-foreground-color "#839496")
 ;; (defvar pos-tip-foreground-color "#839496" "default color for pos tip.")


 (defun aqua/find-file-hook ()
   (unless (eq major-mode 'org-mode)
     (setq show-trailing-whitespace t))
   (when (string-match "\\.min\\." (buffer-file-name))
     (fundamental-mode)))
 (add-hook 'find-file-hook #'aqua/find-file-hook)

 ;;**
 ;;** golden-ration and imenu-list settings (invoke With "-bi")
 (require-package 'golden-ratio)
 (golden-ratio-mode 1)

 (require-package 'imenu-list)
 (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")
 (setq imenu-list-focus-after-activation t
       imenu-list-auto-resize t)

 ;;----------------------------------------------------------------------------
 ;;** HTTP REST client tool for emacs
 ;;** C-c C-c: runs the query under the cursor, tries to pretty-print the response
 ;;----------------------------------------------------------------------------
 (require-package 'restclient)

 ;;----------------------------------------------------------------------------
 ;;** volatile-highlights
 ;;----------------------------------------------------------------------------
 (require 'volatile-highlights)        ;; provide visual feedback
 (volatile-highlights-mode t)

 ;;----------------------------------------------------------------------------
 ;;** hl-sexp: minor mode to highlight s-expression
 ;;----------------------------------------------------------------------------
 (require-package 'hl-sexp)
 (require 'hl-sexp)

 (add-hook 'clojure-mode-hook #'hl-sexp-mode)
 (add-hook 'lisp-mode-hook #'hl-sexp-mode)
 ;; (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)

 ;;----------------------------------------------------------------------------
 ;;** Emacs ElDoc: Display Function or Variable Information Near Point (Cursor)
 ;;----------------------------------------------------------------------------
 (use-package eldoc
   :defer t
   ;; Enable Eldoc for `eval-expression'
   :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
   :config
   (setq eldoc-documentation-function #'describe-char-eldoc)
   :custom
   (eldoc-idle-delay 1))

 (defun aqua/eldoc-display-message (format-string &rest args)
   "Display eldoc message of FORMAT-STRING and REST arguments ARGS near point."
   (when format-string
     (pos-tip-show (apply 'format format-string args))))
 (setq eldoc-message-function #'aqua/eldoc-display-message)

 ;;----------------------------------------------------------------------------
 ;;** Show tooltip with function documentation at point
 ;;** by default clippy uses pos-tip - here we are using popup
 ;;----------------------------------------------------------------------------
 (require 'clippy)
 (after "pos-tip"
   (setq clippy-tip-show-function #'clippy-pos-tip-show))

 ;;----------------------------------------------------------------------------
 ;;** toggle menu-bar, scroll-bar and tool-bar
 ;;----------------------------------------------------------------------------
 (defun aqua/toggle-bars ()
   "Toggles the menu, tool and scroll bars."
   (interactive)
   (if menu-bar-mode
       (progn
         (menu-bar-mode -1)
         (tool-bar-mode -1)
         (scroll-bar-mode -1))
     (progn
       (menu-bar-mode 1)
       (scroll-bar-mode 1)
       (tool-bar-mode 1))))

 ;;----------------------------------------------------------------------------
 ;;** buffer mode
 ;;----------------------------------------------------------------------------
 (require 'buffer-move)
 (global-set-key (kbd "<S-s-up>")     'buf-move-up)
 (global-set-key (kbd "<S-s-down>")   'buf-move-down)
 (global-set-key (kbd "<S-s-left>")   'buf-move-left)
 (global-set-key (kbd "<S-s-right>")  'buf-move-right)

 ;;----------------------------------------------------------------------------
 ;;** rotate the layout of emacs
 ;;----------------------------------------------------------------------------
 (require-package 'rotate)

 ;;**
 ;;** get the buffer file name
 (defmacro buffer-real-name ()
   "Get the real filename of the current buffer without parent directory."
   '(file-name-nondirectory buffer-file-name))

 ;;----------------------------------------------------------------------------
 ;;** load any vendor packages
 ;;----------------------------------------------------------------------------
 ;; (require 'dircolors)                  ;; ls --color inside emacs

 ;;----------------------------------------------------------------------------
 ;;** line numbers
 ;;----------------------------------------------------------------------------
 (if (fboundp #'display-line-numbers-mode)
     (add-hook 'find-file-hook #'display-line-numbers-mode)
   (add-hook 'find-file-hook 'linum-mode))

 ;; highlight current line
 ;; awesome in the terminal version of emacs though, so we don’t use that
 (when window-system (add-hook 'prog-mode-hook 'hl-line-mode))


 ;;----------------------------------------------------------------------------
 ;;** for [text-mode]
 ;;----------------------------------------------------------------------------
 (defvar soft-line-breaks-p nil   ; nil or t
   "Use hard or soft line breaks for long lines")

 ;; M-q doesn't insert double space after period.
 (setq sentence-end-double-space nil)

 (if soft-line-breaks-p
     (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
   (add-hook 'text-mode-hook
             '(lambda ()
                (set-fill-column 78)       ; lines are 78 chars long ...
                (auto-fill-mode t))))      ; ...and wrapped around automagically


 ;;----------------------------------------------------------------------------
 ;;** [Paradox] - a better package menu handler
 ;;----------------------------------------------------------------------------
 (use-package paradox
   :config
   (setq-default
    paradox-column-width-package 27
    paradox-column-width-version 13
    paradox-execute-asynchronously t
    paradox-github-token t
    paradox-hide-wiki-packages t)
   (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

 ;;----------------------------------------------------------------------------
 ;; elisp function and variable help
 ;; http://jasonm23.roughdraft.io/eea3b4662e2a3028a07e-emacs-help-
 ;; popup-for-function-or-variable-at-point
 ;; [C-x R E]
 ;;----------------------------------------------------------------------------
 (defun describe-in-popup (fn)
   "Describe about an function FN."
   (let* ((thing (symbol-at-point))
          (description (save-window-excursion
                         (funcall fn thing) ;; this is the yield point
                         (switch-to-buffer "*Help*")
                         (buffer-string))))
     (popup-tip description
                :point (point)
                :around t
                :height 30
                :scroll-bar t
                :margin t)))

 (defun describe-thing-in-popup ()
   "Interactive command to run the function and pick the correct describe function."
   (interactive)
   (let* ((thing (symbol-at-point)))
     (cond
      ((fboundp thing) (describe-in-popup 'describe-function))
      ((boundp thing) (describe-in-popup 'describe-variable)))))

 (global-set-key (kbd "C-x R E") 'describe-thing-in-popup)


 ;;----------------------------------------------------------------------------
 ;;** code review
 ;;----------------------------------------------------------------------------
 (defun code-review-region (beg end)
   "Copy code region between BEG and END, insert file name, and line number, ready to be pasted into an email."
   (interactive "r")
   (let* ((text (chomp (buffer-substring-no-properties beg end)))
          (line-number (line-number-at-pos))
          (file (buffer-file-name))
          (path (replace-regexp-in-string "^.*branches/" ""
                                          (replace-regexp-in-string
                                           "^.*trunk/" "" file))))
     (with-temp-buffer
       (insert text)
       (goto-char (point-min))
       (while (re-search-forward "^" nil t)
         (replace-match "| " nil nil))
       (goto-char (point-min))
       (insert (format "+---[%s:%s]\n" path line-number))
       (goto-char (point-max))
       (insert "\n+---\n")
       (kill-region (point-min) (point-max)))))

 ;;----------------------------------------------------------------------------
 ;;** Highlight the columns which are longer than 79 columns
 ;;----------------------------------------------------------------------------
 (when (> (display-color-cells) 16)         ;if not in CLI
   (add-hook 'prog-mode-hook
             (lambda ()
               (font-lock-add-keywords nil '(("^[^\n]\\{79\\}\\(.*\\)$" 1 font-lock-warning-face t)))
               (font-lock-add-keywords nil '(("\\<\\(FIXA\\|TEST\\|TODO\\|FIXME\\|BUG\\|NOTE\\)"
                                              1 font-lock-warning-face prepend)))
               (font-lock-add-keywords nil '(("\\<\\(__FUNCTION__\\|__PRETTY_FUNCTION__\\|__LINE__\\)"
                                              1 font-lock-preprocessor-face prepend)))
               )))

 ;;----------------------------------------------------------------------------
 ;;** quick text navigation through AVY
 ;;** and quickly switch windows in Emacs through ace-windows
 ;;----------------------------------------------------------------------------
 (use-package avy
   :ensure t
   :bind
   ("M-y" . avy-goto-char))

 ;; ace-window
 (use-package ace-window
   :ensure t
   :init
   (progn
     (global-set-key [remap other-window] 'ace-window)
     (custom-set-faces
      '(aw-leading-char-face
        ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

 ;;----------------------------------------------------------------------------
 ;;** calendar framework for Emacs
 ;;----------------------------------------------------------------------------
 (use-package calfw       ;Calfw program displays a calendar view in the Emacs buffer
   :commands cfw:open-org-calendar
   :defer 0.5
   :ensure t
   :config
   (progn
     (use-package calfw-org)
     ;; Unicode characters
     (setq cfw:fchar-junction ?╋
           cfw:fchar-vertical-line ?┃
           cfw:fchar-horizontal-line ?━
           cfw:fchar-left-junction ?┣
           cfw:fchar-right-junction ?┫
           cfw:fchar-top-junction ?┯
           cfw:fchar-top-left-corner ?┏
           cfw:fchar-top-right-corner ?┓)))


 ;;----------------------------------------------------------------------------
 ;;** A smart M-x enhancement for Emacs
 ;;----------------------------------------------------------------------------
 (use-package smex
   :defer t
   :init
   (progn
     (setq-default smex-history-length 32
                   smex-save-file (concat cache-dir "/.smex-items"))))


 ;;----------------------------------------------------------------------------
 ;;** [outshine] - bring org-mode look and feel for all buffers
 ;;----------------------------------------------------------------------------
 ;;(require-package 'outshine)
 ;;(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
 ;; add to major modes...
 ;; (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
 ;; (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
 ;; (add-hook 'clojure-mode-hook 'outline-minor-mode)

 ;;**
 ;; pdf tools for viewing and interacting with pdf
 (use-package pdf-tools
   :ensure t
   :mode ("\\.pdf\\'" . pdf-tools-install)
   :bind ("C-c C-g" . pdf-sync-forward-search)
   :defer t
   :config
   (setq mouse-wheel-follow-mouse t)
   (setq pdf-view-resize-factor 1.10))

 ;;**
 ;; highlights bad word choices and has functions for calculating readability
 (use-package writegood-mode
   :ensure t
   :bind ("C-c g" . writegood-mode)
   :config
   (add-to-list 'writegood-weasel-words "actionable"))

 ;;**
 ;; auto-compile Emacs lisp libraries
 (use-package auto-compile
   :defer nil
   :config
   (auto-compile-on-load-mode)
   ;;(auto-compile-on-save-mode)
   (setq auto-compile-display-buffer t
	 auto-compile-use-mode-line nil))


 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'utils-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; utils-config.el ends here
