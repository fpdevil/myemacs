; ~/.emacs.d/my-internals.el


(global-set-key (kbd "<menu>") 'nil)

;;********************************************************
;; some customizations
;;********************************************************
(setq user-full-name "Sampath Singamsetty"
      user-mail-address "Singamsetty.Sampath@gmail.com")

(show-paren-mode t)

(recentf-mode 1)

(global-set-key "\M- " 'hippie-expand)

(setq truncate-lines t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Quit Aquamacs? "))
            kill-emacs-query-functions))

(put 'upcase-region 'disabled nil)



;;
; Saving Emacs Sessions
;;
(desktop-save-mode nil)
;(setq desktop-restore-eager 10)
(setq desktop-save nil) ;; save without asking

(defalias 'list-buffers 'ibuffer)

;;
; electric pair mode
;;
; (electric-pair-mode 1)
; (defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
; (defun markdown-add-electric-pairs ()
;   (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
;   (setq-local electric-pair-text-pairs electric-pair-pairs))
; (add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)


;;
; disable electric indent mode to prevent auto indentation
;;
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))


(global-set-key (kbd "<C-S-up>")     'windmove-up)
(global-set-key (kbd "<C-S-down>")   'windmove-down)
(global-set-key (kbd "<C-S-left>")   'windmove-left)
(global-set-key (kbd "<C-S-right>")  'windmove-right)

;; set default font
(set-default-font "Monaco for Powerline")

(defun timestamp ()
  (interactive)
  (insert (format-time-string "%d.%m.%Y, %H:%M")))

(defun my-count-words-region (posBegin posEnd)
  "Print number of words and chars in region."
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
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

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

;;
; colorize the output of the compilation mode.
;;
; (require 'ansi-color)
; (defun colorize-compilation-buffer ()
;   (toggle-read-only)
;   (ansi-color-apply-on-region (point-min) (point-max))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  ;; mocha seems to output some non-standard control characters that
  ;; aren't recognized by ansi-color-apply-on-region, so we'll
  ;; manually convert these into the newlines they should be.
  (goto-char (point-min))
  (while (re-search-forward "\\[2K\\[0G" nil t)
    (progn
      (replace-match "
")))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;
; rainbow identifier customizations
; customized filter: don't mark *all* identifiers
;;
(defun rainbow-identifiers-filter (beg end)
  "Only highlight standalone words or those following 'this.' or 'self.'"
  (let ((curr-char (char-after beg))
        (prev-char (char-before beg))
        (prev-self (buffer-substring-no-properties
                    (max (point-min) (- beg 5)) beg)))
    (and (not (member curr-char
                    '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ??)))
         (or (not (equal prev-char ?\.))
             (equal prev-self "self.")
             (equal prev-self "this.")))))

;; Filter: don't mark identifiers inside comments or strings
(setq rainbow-identifiers-faces-to-override
      '(font-lock-type-face
        font-lock-variable-name-face
        font-lock-function-name-face))

;; Set the filter
(add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)

;;
; for python process in inferior mode run
;;
(defun run-python-once ()
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python (python-shell-parse-command)))

(add-hook 'python-mode-hook 'run-python-once)