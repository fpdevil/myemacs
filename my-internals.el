;;; package --- customized settings
;;;
;;; Commentary:
;;; filename.  : ~/.emacs.d/my-internals.el
;;; description: contains genral customizations which cannot be placed anywhere
;;;
;;; Code:
;;;
(global-set-key (kbd "<menu>") 'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          some customizations                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Sampath Singamsetty"
      user-mail-address "Singamsetty.Sampath@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; garbage collection setup for aquamacs                                   ;;
;; Start garbage collection every 100MB to improve Emacs performance       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold 100000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parentheses show                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get visual indication of an exception                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set 'visible-bell t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent files list                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(recentf-mode 1)

(global-set-key "\M- " 'hippie-expand)

(setq truncate-lines t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Quit Aquamacs? "))
            kill-emacs-query-functions))

(put 'upcase-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax highlighting everywhere                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add proper word wrapping                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-visual-line-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for pdf viewing                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq doc-view-continuous t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving Emacs Sessions                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq desktop-save nil) ;; save without asking
;(setq desktop-restore-eager 10)

;;
; (defalias 'list-buffers 'ibuffer)

;;
; electric pair mode
;;
; (electric-pair-mode 1)
; (defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
; (defun markdown-add-electric-pairs ()
;   (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
;   (setq-local electric-pair-text-pairs electric-pair-pairs))
; (add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable electric indent mode to prevent auto indentation                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keybinding stuff                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)
; (global-set-key (kbd "RET") 'newline-and-indent)
; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
; (global-set-key (kbd "C-c C-k") 'compile)
; (global-set-key (kbd "C-x g") 'magit-status)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set default font for aquamacs                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil :family "Monaco"
            		                 :width 'normal
                                 :height 120
                                 :weight 'ultralight)
(set-face-bold-p 'bold nil)             ;; disable bold fonts
(setq mac-allow-anti-aliasing t)        ;; anti-aliasing
;;
; (set-face-attribute 'default nil :family "Monaco for Powerline"
;                                  :height 120
;                                  :weight 'ultra-light)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get default timestamp format                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun timestamp ()
  "Latest timestamp."
  (interactive)
  (insert (format-time-string "%d.%m.%Y, %H:%M")))

(defun my-count-words-region (posBegin posEnd)
  "POSBEGIN POSEND Print number of words and chars in region."
  (interactive "r")
  (message "Counting …")
  (save-excursion
    (let (wordCount charCount)
      (setq wordCount 0)
      (setq charCount (- posEnd posBegin))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\w+\\W*" posEnd t))
        (setq wordCount (1+ wordCount)))
      (message "Words: %d. Chars: %d." wordCount charCount)
      )))


(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))


(defun unfill-region (start end)
  "START END Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for latex editing                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq LaTeX-math-menu-unicode t)
; (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
; (require 'reftex)
; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
; (setq reftex-plug-into-AUCTeX t)
; (setq-default TeX-master nil)
; (setq reftex-cite-format 'natbib)
(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; colorize the output of the compilation mode.                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow identifier customizations                                       ;;
;; customized filter: don't mark *all* identifiers                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rainbow-identifiers-filter (beg end)
  "BEG END Only highlight standalone words or those following 'this.' or 'self.'."
  (let ((curr-char (char-after beg))
        (prev-char (char-before beg))
        (prev-self (buffer-substring-no-properties
                    (max (point-min) (- beg 5)) beg)))
    (and (not (member curr-char
                    '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ??)))
         (or (not (equal prev-char ?\.))
             (equal prev-self "self.")
             (equal prev-self "this.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filter: don't mark identifiers inside comments or strings               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rainbow-identifiers-faces-to-override
      '(font-lock-type-face
        font-lock-variable-name-face
        font-lock-function-name-face))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the filter                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Mode Hooks                                                  ;;
;; make sure things like FIXME and TODO are highlighted so they stand out  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO."
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

(add-hook 'prog-mode-hook #'add-watchwords)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display world times of interest with M-x display-time-world             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-time-world-list
      '(("Asia/India" "India")
        ("America/Chicago" "Chicago")
        ("America/Los_Angeles" "Los Angeles")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for flyspell (spell checking)                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-hook 'text-mode-hook 'flyspell-mode)
; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
; (add-hook 'org-mode-hook 'flyspell-mode)
; (add-hook 'markdown-mode-hook 'flyspell-mode)
; (setq-default ispell-program-name "/usr/local/bin/ispell")


(provide 'my-internals)

;;; my-internals.el ends here
