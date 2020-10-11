;;; package --- custom settings under aqua-customizations.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-customizations.el
;;; description: Contains general Aquamacs custom options
;;;              This is the place where Aquamacs default preferences are
;;;              changed or altered.
;;; Code:
;;;


;;------------------------------------------------------------------------------
;;** utf-8 character set encoding and Locale
;;------------------------------------------------------------------------------
(prefer-coding-system         'utf-8)
(set-default-coding-systems   'utf-8)

;;------------------------------------------------------------------------------
;;** proper default language setup [EN]
;;------------------------------------------------------------------------------
(setq current-language-environment "English")

;;------------------------------------------------------------------------------
;;** auto-indent on RET
;;------------------------------------------------------------------------------
;;(define-key global-map (kbd "RET") 'newline-and-indent)

;;------------------------------------------------------------------------------
;;** suppress gui features
;;------------------------------------------------------------------------------
;;(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-default-init t)
(setq inhibit-startup-screen nil) ;; disable startup, splash screen and startup message
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-echo-area-message nil)
(setq initial-buffer-choice t)
(setq initial-major-mode 'fundamental-mode)

;;------------------------------------------------------------------------------
;;** display an initial scratch message & prettify symbols
;;------------------------------------------------------------------------------
; (setq initial-scratch-message nil)
; (setq initial-scratch-message
;         (concat "ॐ  Emacs With ❤️ " user-login-name "!\n" "☆ సంపత్ కుమార్ ☆" "\n"))

;;------------------------------------------------------------------------------
;;** show a marker in the left fringe for lines not in the buffer
;;------------------------------------------------------------------------------
(setq indicate-empty-lines t)

;;------------------------------------------------------------------------------
;;** Visually indicate buffer boundaries and scrolling in the fringe
;;------------------------------------------------------------------------------
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))

;;------------------------------------------------------------------------------
;;** Abbrev Mode - Saving abbreviations at custom location
;;------------------------------------------------------------------------------
(setq abbrev-file-name (expand-file-name "abbrev_defs" cache-dir))

;; abbrev mode for text mode
(add-hook 'text-mode-hook 'abbrev-mode)

;;------------------------------------------------------------------------------
;;** escape save question during exit for custom settings
;;------------------------------------------------------------------------------
(setq aquamacs-save-options-on-quit nil)

;;------------------------------------------------------------------------------
;;** save and load the mini buffer history
;;------------------------------------------------------------------------------
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-file (expand-file-name "minibuffer-history.el" cache-dir))
;; (when (file-exists-p savehist-file) (load savehist-file))

(setq history-length 100)
(setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

;; aquamacs custom variables
(setq desktop-path (quote ("~/.emacs.d" "." "~")))
(setq aquamacs-autosave-directory (expand-file-name "AutoSave" cache-dir))
(setq aquamacs-scratch-file (expand-file-name "scratch buffer" preferences-dir))
(setq ede-simple-save-directory (expand-file-name "EDE" preferences-dir))
;;(setq save-frame-position-file (expand-file-name "frame-positions.el" preferences-dir))
;;(setq save-place-file (expand-file-name "places.el" preferences-dir))

;;------------------------------------------------------------------------------
;;** save-list-file customizations
;;------------------------------------------------------------------------------
(let ((dir (expand-file-name (concat cache-dir "/auto-save-list/"))))
  (setq auto-save-list-file-prefix (concat dir "saves-"))
  (setq auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;;------------------------------------------------------------------------------
;;** for better scrolling
;;------------------------------------------------------------------------------
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 3)

;;------------------------------------------------------------------------------
;;** [Emoji] - Enable and stop the UI from freezing when trying to display them.
;;------------------------------------------------------------------------------
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


;;------------------------------------------------------------------------------
;;** [Emacs] - aquamacs emacs compatibility settings
;;   display standard Emacs (and not standard Mac) modifier symbols)
;;------------------------------------------------------------------------------
(setq ns-use-mac-modifier-symbols  nil)
(setq select-enable-clipboard t)   ; exchanging clipboard content with other applications

;;------------------------------------------------------------------------------
;;** replace region when typing
;;------------------------------------------------------------------------------
;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.  Also, commands that normally delete
;; just one character will delete the entire selection instead.
(delete-selection-mode 1)

;;----------------------------------------------------------------------------
;;**  Programming Mode Hooks
;;*** make sure things like FIXME and TODO are highlighted so they stand out
;;----------------------------------------------------------------------------
(defun add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO."
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

;;(add-hook 'prog-mode-hook #'add-watchwords)

(defun annotate-todo ()
  "Put a fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

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



;;;; buffer menu highlighting
(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ; Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)))     ; Read only

(defun buffer-menu-custom-font-lock  ()
      (let ((font-lock-unfontify-region-function
             (lambda (start end)
               (remove-text-properties start end '(font-lock-face nil)))))
        (font-lock-unfontify-buffer)
        (set (make-local-variable 'font-lock-defaults)
             '(buffer-menu-buffer-font-lock-keywords t))
        (font-lock-fontify-buffer)))

(add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)


(provide 'aqua-customizations)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-customizations.el ends here
