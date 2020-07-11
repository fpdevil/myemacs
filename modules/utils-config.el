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

;;* all utilities section
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

 ;;--------------------------------------------------------------------------
 ;;** HTTP REST client tool for emacs
 ;;** C-c C-c: runs query under the cursor, tries to pretty-print response
 ;;--------------------------------------------------------------------------
 (require-package 'restclient)

 ;;--------------------------------------------------------------------------
 ;;** volatile-highlights
 ;;--------------------------------------------------------------------------
 (require 'volatile-highlights)        ;; provide visual feedback
 (volatile-highlights-mode t)

 ;; extensions to vhl
 ;; for vip
 (vhl/define-extension 'vip 'vip-yank)
 (vhl/install-extension 'vip)

 ;; for undo-tree
 (after "undo-tree"
   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
   (vhl/install-extension 'undo-tree))

 ;;--------------------------------------------------------------------------
 ;;** hl-sexp: minor mode to highlight s-expression
 ;;--------------------------------------------------------------------------
 ;;(require-package 'hl-sexp)
 ;;(require 'hl-sexp)
 ;;(add-hook 'clojure-mode-hook #'hl-sexp-mode)
 ;;(add-hook 'lisp-mode-hook #'hl-sexp-mode)
 ;; (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)

 ;;--------------------------------------------------------------------------
 ;;** code folding
 ;;--------------------------------------------------------------------------
 (defun turn-on-hideshow () (hs-minor-mode 1))
 (add-hook 'python-mode-hook 'turn-on-hideshow)
 ;;(add-hook 'prog-mode-hook #'hs-minor-mode)

 ;;--------------------------------------------------------------------------
 ;;** Emacs ElDoc: Display Function or Variable Information Near Point (Cursor)
 ;;--------------------------------------------------------------------------
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
 ;; (setq eldoc-message-function #'aqua/eldoc-display-message)

 ;;--------------------------------------------------------------------------
 ;;** Show tooltip with function documentation at point
 ;;** by default clippy uses pos-tip - here we are using popup
 ;;--------------------------------------------------------------------------
 (require 'clippy)
 (after "pos-tip"
   (setq clippy-tip-show-function #'clippy-pos-tip-show))

 ;;--------------------------------------------------------------------------
 ;;** buffer mode
 ;;--------------------------------------------------------------------------
 (require 'buffer-move)
 (global-set-key (kbd "<S-s-up>")     'buf-move-up)
 (global-set-key (kbd "<S-s-down>")   'buf-move-down)
 (global-set-key (kbd "<S-s-left>")   'buf-move-left)
 (global-set-key (kbd "<S-s-right>")  'buf-move-right)

 ;;--------------------------------------------------------------------------
 ;;** rotate the layout of emacs
 ;;--------------------------------------------------------------------------
 (require-package 'rotate)

 ;;--------------------------------------------------------------------------
 ;;** get the buffer file name
 ;;--------------------------------------------------------------------------
 (defmacro buffer-real-name ()
   "Get the real filename of the current buffer without parent directory."
   '(file-name-nondirectory buffer-file-name))

 ;;--------------------------------------------------------------------------
 ;;** load any vendor packages
 ;;--------------------------------------------------------------------------
 ;; (require 'dircolors)                  ;; ls --color inside emacs

 ;;--------------------------------------------------------------------------
 ;;** line numbers (on the left by default)
 ;;--------------------------------------------------------------------------
 ;; (if (fboundp #'display-line-numbers-mode)
 ;;     (add-hook 'find-file-hook #'display-line-numbers-mode)
 ;;   (add-hook 'find-file-hook 'linum-mode))


 ;; -- highlight the current line
 ;; awesome in the terminal version of emacs though, so we don’t use that
 (when window-system (add-hook 'prog-mode-hook 'hl-line-mode))


 ;;--------------------------------------------------------------------------
 ;;** for [text-mode]
 ;;--------------------------------------------------------------------------
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

 ;;--------------------------------------------------------------------------
 ;;** [Paradox] - a better package menu handler
 ;;--------------------------------------------------------------------------
 (use-package paradox
   :config
   (setq-default
    paradox-column-width-package 27
    paradox-column-width-version 13
    paradox-execute-asynchronously t
    paradox-github-token t
    paradox-hide-wiki-packages t)
   (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

 ;;--------------------------------------------------------------------------
 ;; indentation highlighting
 ;;--------------------------------------------------------------------------
(use-package highlight-indent-guides
  :defer t
  :hook ((prog-mode . highlight-indent-guides-mode))
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))


 ;;--------------------------------------------------------------------------
 ;; elisp function and variable help
 ;; http://jasonm23.roughdraft.io/eea3b4662e2a3028a07e-emacs-help-
 ;; popup-for-function-or-variable-at-point
 ;; [C-x R E]
 ;;--------------------------------------------------------------------------
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


 ;;--------------------------------------------------------------------------
 ;;** code review
 ;;--------------------------------------------------------------------------
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

 ;;--------------------------------------------------------------------------
 ;;** quick text navigation through AVY
 ;;** and quickly switch windows in Emacs through ace-windows
 ;;--------------------------------------------------------------------------
 (use-package avy
   :defer 0.5
   :ensure t
   :bind
   ("M-y" . avy-goto-char))

 ;; ace-window
 (use-package ace-window
   :defer 0.5
   :ensure t
   :init
   (progn
     (global-set-key [remap other-window] 'ace-window)
     (custom-set-faces
      '(aw-leading-char-face
        ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

 ;;--------------------------------------------------------------------------
 ;;** calendar framework for Emacs
 ;;--------------------------------------------------------------------------
 (use-package calfw       ;Calfw program displays a calendar view in the Emacs buffer
   :commands cfw:open-org-calendar
   :defer t
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


 ;;--------------------------------------------------------------------------
 ;;** A smart M-x enhancement for Emacs
 ;;--------------------------------------------------------------------------
 (use-package smex
   :defer t
   :init
   (progn
     (setq-default smex-history-length 32
                   smex-save-file (concat cache-dir "/.smex-items"))))

 ;;--------------------------------------------------------------------------
 ;;** highlights bad word choices and has functions for calculating readability
 ;;--------------------------------------------------------------------------
 (use-package writegood-mode
   :defer t
   :ensure t
   :bind ("C-c g" . writegood-mode)
   :config
   (add-to-list 'writegood-weasel-words "actionable"))

 ;;--------------------------------------------------------------------------
 ;;** auto-compile Emacs lisp libraries
 ;;--------------------------------------------------------------------------
 (use-package auto-compile
   :demand t
   :config
   (auto-compile-on-load-mode)
   (auto-compile-on-save-mode)
   (setq auto-compile-display-buffer               nil)
   (setq auto-compile-mode-line-counter            t)
   (setq auto-compile-source-recreate-deletes-dest t)
   (setq auto-compile-toggle-deletes-nonlib-dest   t)
   (setq auto-compile-update-autoloads             t)
   (add-hook 'auto-compile-inhibit-compile-hook
             'auto-compile-inhibit-compile-detached-git-head))

 ;;--------------------------------------------------------------------------
 ;;** display tildes in the fringe for empty buffers similar to ViM
 ;;--------------------------------------------------------------------------
 (use-package vi-tilde-fringe
   :defer t
   :init
   (global-vi-tilde-fringe-mode))

 ;;--------------------------------------------------------------------------
 ;; restart emacs as needed
 ;;--------------------------------------------------------------------------
 (use-package restart-emacs
   :defer t)

 ;;--------------------------------------------------------------------------
 ;;** call whitespace cleanup on save for program buffers
 ;;--------------------------------------------------------------------------
 (use-package whitespace-cleanup-mode
   :ensure t
   :config
   (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

 ;;--------------------------------------------------------------------------
 ;;** check if the parens match in a programming mode
 ;;--------------------------------------------------------------------------
 (defun aqua/check-parens ()
   "Check for matching parens in programming modes."
   (when after-init-time  ;; this allows not to check parens while initializing
     (condition-case err
         (check-parens)
       ((error) (message "[ERROR] buffer has unmatched parentheses or a quote")))))
 (add-hook 'prog-mode-hook 'aqua/check-parens t)

 ;;--------------------------------------------------------------------------
 ;;** Indent Tools (yaml, python, jade, etc)
 ;; Indent,  move around  and  act  on code  based  on indentation
 ;;--------------------------------------------------------------------------
 (require-package 'indent-tools)
 (add-hook 'python-mode-hook
           (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body)))

 ;;--------------------------------------------------------------------------
 ;;** Move Text/Regio
 ;;--------------------------------------------------------------------------
 (defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

 (defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

 (defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

 (global-set-key [\M-\S-up] 'move-text-up)
 (global-set-key [\M-\S-down] 'move-text-down)

 (require 'drag-stuff)
 (drag-stuff-mode t)

 )



(provide 'utils-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; utils-config.el ends here
