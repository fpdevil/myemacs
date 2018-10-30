;;; package --- configuration settings for Emacs ipython notebook
;;;
;;; Commentary:
;;;
;;; Filename   : ipythonb-config.el
;;; Description: Emacs ipython notebook
;;;
;;; elisp code for customizing python code
;;;
;;; Code:

;; Specify the jupyter executable name, and the start dir of the server
(defvar my:jupyter_location (executable-find "jupyter"))
(defvar my:jupyter_start_dir "~/sw/programming/python")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein - ipython notebooks in gui emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only launch if the executable exists.
(if (and my:jupyter_location
         my:jupyter_start_dir)
    (use-package ein
      :ensure t
      :defer t
      :config
      (setq ein:jupyter-default-notebook-directory my:jupyter_start_dir
            ein:jupyter-default-server-command my:jupyter_location
            ein:jupyter-server-args (list "--no-browser")
            ein:use-auto-complete-superpack t
            ein:completion-backend 'ein:use-ac-jedi-backend)
      (progn
        ;; We need to load additional packages for ein to work correctly when
        ;; in  byte-compiled mode. The  reason for this is that we do not use
        ;; package.el to  initialize packages and so the EIN dependencies are
        ;; not configured  properly. We  will first search  for the directory
        ;; ".*/ein-.*" in the load-path. Once we found this entry we list all
        ;; .el files in the directory, remove the extension and then load the
        ;; ones that are required. The non-required ones are listed below:
        ;;   ein - can't depend on ourselves
        ;;   debug-ein - we are not debugging
        ;;   ein-pkg - we don't have package.el loaded, so RIP
        (dolist (my:current-path load-path)
          (if (string-match-p "/ein-" my:current-path)
              (dolist (name (directory-files my:current-path nil "\\.el$"))
                (if (and (not (equal "ein" (file-name-sans-extension name)))
                         (not (equal "debug-ein" (file-name-sans-extension name)))
                         (not (equal "ein-pkg" (file-name-sans-extension name))))
                    (load (file-name-sans-extension name) nil t)))))
        ;;(ein:jupyter-server-start my:jupyter_location my:jupyter_start_dir)
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ipythonnb-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ipythonnb-config.el ends here
