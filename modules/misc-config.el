;;; package  --- misc-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : misc-config.el
;;; Description: Miscellaneous bootstrap ettings to operate over Emacs and other
;;; modes as needed
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- use this for conditional loding of the features (credit Schcha Chuha)
;;; == (@ref https://github.com/bling/dotemacs.git)
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1))
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.

FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

;;; -- load lazily or initialize lazily
(defmacro lazy-init (&rest body)
  "Initializae the BODY after being idle for a predetermined amount of time."
  `(run-with-idle-timer
    0.5
    nil
    (lambda () ,@body)))

;; == shortcut with alphabet
(setq-default switch-window-shortcut-style 'alphabet)
;; == control cancel switching after timeout
(setq-default switch-window-timeout nil)
;; == usage shortcut for window switch
(global-set-key (kbd "C-x o") 'switch-window)


;;; -- suppress gui features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;; == show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;;; -- display an initial scratch message & prettify symbols
(setq-default initial-scratch-message
              (concat "ॐ  Emacs With ❤️ " user-login-name "!\n" "☆ సంపత్ కుమార్ ☆" "\n"))

;;; -- prettify the symbols
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode)
  ;;(add-hook 'js2-mode-hook
  ;;          (lambda ()
  ;;            (push '("function" . 955) prettify-symbols-alist)
  ;;            (push '("return" . 8592) prettify-symbols-alist)))
  )

;;; -- elisp bench marking - check time taken to load each component
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
  (message "[INITIALIZATION] completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))
;; (add-hook 'after-init-hook 'sanityinc/show-init-time)

;;; -- activate benchmarking...
(benchmark-init/activate)

;;;-- [TAB] key settings - handle whitespaces
(require 'whitespace)
(setq whitespace-style
      '(face tabs empty trailing))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(set-face-attribute 'whitespace-line nil
                    :foreground "#880"
                    :background nil
                    :weight 'bold)
(set-face-attribute 'whitespace-trailing nil
                    :background "#FDD")


;;; -- check the buffer file name
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

;;; -- utility to get list of minor modes
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


;;; -- add or disable a specific backend in company-backends
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

;;; -- indentation function
(defun hindent-reformat-buffer-on-save ()
  "Indent an entire buffer with the default indentation scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;;----------------------------------------------------------------------------
;; do not annoy with those messages about active processes while exitting
;;----------------------------------------------------------------------------
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;----------------------------------------------------------------------------
;;; -- auto save the undo-tree history
;;----------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)                         ;; enable undo-tree globally
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; misc-config.el ends here
