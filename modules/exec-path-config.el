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

; (after 'exec-path-from-shell
;   '(progn
;      (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
;        (add-to-list 'exec-path-from-shell-variables var)))
;   (setq exec-path-from-shell-check-startup-files nil)
;   (when (memq window-system '(mac ns))
;     (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :custom
  (shell-file-name "/bin/zsh")
  (exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
  :init
  (if (string-equal system-type "darwin")
      (exec-path-from-shell-initialize)))


;;;;;;;;;;;;;;;;;;;;;;; exec path configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'exec-path-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; exec-path-config.el ends here
