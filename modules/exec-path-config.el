;;; package --- exec-path-config.el
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : exec-path-config.el
;;; Description: Emacs settings for setting path
;;;
;;; elisp code for customizing the Emacs exec path
;;;
;;; Code:
;;;

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :custom
  (shell-file-name "/bin/zsh")
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "PKG_CONFIG_PATH"
                                    "RUST_SRC_PATH"))
  :config
  (exec-path-from-shell-initialize))

(provide 'exec-path-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; exec-path-config.el ends here
