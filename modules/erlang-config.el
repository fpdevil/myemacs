;;; package --- Erlang Configuration
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename: erlang-config.el
;;; Description: A major mode erlang language support in Emacs
;;; Configuration with Distel,
;;;                    Esense (Intelligent Code Completion) and
;;;                    EDTS
;;;
;;; elisp code for erlang language support and handling
;; we need to find the paths to OTP, distel and esense
;;
;; for OTP, we need the dir containing man and lib.
;; on debian, that would be /usr/lib/erlang
;; for esense, we need the dir where esense.el lives
;; e.g. $HOME/code/esense-1.9
;; for distel, we need distel.el
;; e.g. $HOME/jungerl/lib/distel/elisp
;; you also need to add the corresponding bit to $HOME/.erlang
;; code:add_patha(os:getenv("HOME")++"/jungerl/lib/distel/ebin").
;;;
;;; DISTEL BUILD
;;;$ svn co http://distel.googlecode.com/svn/trunk/ distel
;;;$ cd distel
;;;$ make
;;;$ cd doc
;;;$ make postscript && make postscript # must run twice
;;;$ make info && sudo make install # install the Info documentation
;;;$ info distel # read the distel info documentation
;;;
;;; Code:
;;;
;;=================================================================================

;; start the erlang mode
(require 'erlang-start)

;;{{{ associate file extensions with major-modes
(add-to-list 'auto-mode-alist '("\\.erl?$"        . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$"        . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.app\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.es\\'"      . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.xrl\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.yrl\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".[eh]rl'"        . erlang-mode))
(add-to-list 'auto-mode-alist '(".yaws?'"         . erlang-mode))
(add-to-list 'auto-mode-alist '(".escript?'"      . erlang-mode))
;;}}}


;;{{{ utility function to handle versioned dirs
(defun wild (dir stem)
  "Return the last (alphabetically) filename that match DIR/STEM*."
  (car
   (reverse
    (sort
     (let (value)
       (dolist (element (file-name-all-completions stem dir) value)
         (setq value (cons (concat dir element) value)))) 'string-lessp))))
;;}}}

;;{{{ setup the locations for erlang root and other packages
(defvar erlang-erl-path "/usr/local/opt/erlang/lib/erlang")
(setq erlang-root-dir erlang-erl-path)
;;find and load the erlang mode .el file
(defvar erlang-erlmode-path
  (concat (wild (concat erlang-erl-path "/lib/") "tools-") "emacs"))
;; (add-to-list 'load-path erlang-erlmode-path)           ; add erlang tools to the path
                                        ;}}}


;;{{{ setup the erlang path for Wrangler and start Wrangler, load the graphviz
;; (defvar erlang-wrangler-path "/usr/local/lib/erlang/lib/wrangler-1.2.0/elisp")
;; (unless (member erlang-wrangler-path load-path)
;;   (add-to-list 'load-path erlang-wrangler-path))
;; GraphViz from Wrangler package
;; (add-hook 'erlang-mode-hook 'erlang-wrangler-on)
;; some wrangler functionalities generate a .dot file and in order
;; to compile the same and view in graphviz specify the below.
;; (load-file (concat erlang-wrangler-path "/graphviz-dot-mode.el"))
                                        ;}}}

;;{{{ ERLANG IDE with EDTS
;;    EDTS - Erlang Development Tool Suite
;;    start edts - place the configuration file .edts in the project
(add-to-list 'safe-local-variable-values '(erlang-indent-level . 2)) ; code indentation
(setq edts-code-issue-wrap-around t)
(defun start-edts ()
  "Initialize EDTS for ERLANG."
  (interactive)
  ;; Set the manual directory and indent level
  (setq edts-man-root erlang-root-dir
        erlang-indent-level 2)
  (setq edts-log-level 'debug)
  (require 'edts-start))

(add-hook 'erlang-mode-hook 'start-edts)
                                        ;}}}

;;{{{ Erlang distel
(defvar erlang-distel-path "/opt/erlang/distel/elisp")
(unless (member erlang-distel-path load-path)
  ;; add distel to end of load path
  (setq load-path (append load-path (list erlang-distel-path))))
                                        ;}}}

;;{{{ erlang Esense
(defvar erlang-esense-path (wild "/opt/erlang/" "esense-"))
(unless (member erlang-esense-path load-path)
  (add-to-list 'load-path erlang-esense-path))
                                        ;}}}


;;{{{ for erlang man pages
(setq erlang-man-root-dir (expand-file-name "man" erlang-erl-path))
(setq erlang-man-dirs (list '("Man - Commands" "lib/erlang/man/man1" t)
                            '("Man - Modules" "lib/erlang/man/man3" t)
                            '("Man - Files" "lib/erlang/man/man4" t)
                            '("Man - Applications" "lib/erlang/man/man6" t)))
                                        ;}}}

;;{{{ for imenu and start an erlang shell with boot flags
(defun my-erlang-hook-function ()
  "Function to add iMenu."
  (interactive)
  (imenu-add-to-menubar "Functions"))
(add-hook 'erlang-mode-hook 'my-erlang-hook-function)
                                        ;}}}

;;{{{ A number of the erlang-extended-mode key bindings are useful in the shell too
;;    setup shortcut keys
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind))
  "Additional keys to bind when in Erlang shell.")
                                        ;}}}

;;{{{ Erlang Distel setup
(require 'distel)
(add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq distel-modeline-node "distel")
            ;; When starting an Erlang shell in Emacs, default in
            ;; the node name.  Pass -name, not -sname so you
            ;; could talk to this node from other machines.
            ;; (setq inferior-erlang-machine-options '("-name" "emacs"))
            (setq inferior-erlang-machine-options '("-sname" "emacs"))
            (setq erlang-compile-extra-opts (list 'debug_info))
            ;; when loading a beam file from emacs, add the path to erlang
            (setq erl-reload-dwim t)))

(distel-setup)

(defvar *erlang-shell-distel-keys* '(erl-complete
                                     erl-find-source-under-point
                                     erl-find-source-unwind
                                     erl-process-list
                                     erl-ie-show-session
                                     erl-fdoc-describe
                                     erl-fdoc-apropos
                                     erl-who-calls
                                     erl-openparen)
  "Distel functions useful in the Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          ;; Add Distel bindings to the Erlang shell.
          (lambda ()
            ;; Comint binding that conflicts with some of the
            ;; distel keybindings.
            (define-key erlang-shell-mode-map (kbd "C-c C-d") nil)

            (loop for (key-binding function) in distel-keys
                  when (memq function *erlang-shell-distel-keys*)
                  do (progn
                       (message "Adding keybinding %s for %s"
                                (format-kbd-macro key-binding) function)
                       (define-key erlang-shell-mode-map key-binding function)))))
                                        ;}}}


;;{{{  auto-complete-mode so can interact with inferior erlang and
;;     popup completion turn on when needed.
(add-hook 'erlang-mode-hook
          (lambda () (auto-complete-mode 1)))

;; -- setup auto complete for distel
;;    erlang ide set-up and erlang auto-completion using auto-complete and distel
(after 'auto-complete
  (require 'auto-complete-distel)
  ;;(setq ac-modes (append ac-modes '(erlang-mode)))
  ;;(setq ac-modes (append ac-modes '(erlang-shell-mode)))
  (setq ac-modes (append ac-modes (list 'erlang-mode)))
  (setq ac-modes (append ac-modes (list 'erlang-shell-mode))))
                                        ;}}}


;;{{{ setup company completions with distel
;;    erlang auto completion using company mode and distel
(after "company"
  (require 'company-distel)
  (add-to-list (make-local-variable 'company-backends)
               'company-distel)

  ;; render company's doc-buffer (default <F1> when on a completion-candidate)
  ;; in a small popup (using popup.el) instead of showing the whole help-buffer.
  (setq company-distel-popup-help t)
  ;; specify the height of the help popup created by company
  (setq company-distel-popup-height 30)
  ;; get documentation from internet
  (setq distel-completion-get-doc-from-internet t)
  ;; Change completion symbols
  (setq distel-completion-valid-syntax "a-zA-Z:_-"))
                                        ;}}}

;;{{{ For ESENSE
(defun init-esense ()
  "Set esense configuration."
  (require 'esense-start)
  (setq esense-indexer-program (concat erlang-esense-path "esense.sh"))
  (setq esense-setup-otp-search-directories t)
  (setq esense-module-search-directories (concat erlang-root-dir "/lib/*/src"))
  (setq esense-include-search-directories (concat erlang-root-dir "/lib/*/include"))
  (setq esense-distel-node "distel@apple"))

(init-esense)

;; Initialize Esense
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun my-erlang-mode-hook ()
  "Erlang hook for Esense Mode."
  ;; (esense-mode)
  ;; distel The Right Way(tm)
  (local-set-key [(meta l)] 'erl-find-mod)
  (local-set-key [(meta \()] 'erl-openparent)
  (local-set-key [(meta /)]  'erl-complete)
  (local-set-key [(control x) (\?)] 'erlang-man-function)
  ;; make hack for compile command
  ;; uses Makefile if it exists, else looks for ../inc & ../ebin
  (unless (null buffer-file-name)
    (make-local-variable 'compile-command)
    (setq compile-command
          (if (file-exists-p "Makefile") "make -k"
            (concat
             (concat
              "erlc "
              (concat
               (if (file-exists-p "../ebin") "-o ../ebin " "")
               (if (file-exists-p "../inc") "-I ../inc " ""))
              "+debug_info -W " buffer-file-name))))))
                                        ;}}}

;;{{{ Initialize comint mode
(add-hook 'comint-mode-hook 'my-comint)
(defun my-comint ()
  "Try to make the shell more like the real shell."
  (local-set-key [tab] 'comint-dynamic-complete)
  (local-set-key [(control up)] 'previous-line)
  (local-set-key [(control down)] 'next-line)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input))
                                        ;}}}

;;{{{ Setup the Erlang Shell
(add-hook 'erlang-shell-mode-hook 'my-erlang-shell)
(defun my-erlang-shell ()
  "Erlang shell handler."
  (setq comint-dynamic-complete-functions
        '(my-erl-complete  comint-replace-by-expanded-history)))

(defun my-erl-complete ()
  "Call erl-complete if we have an Erlang node name."
  (if erl-nodename-cache
      (erl-complete erl-nodename-cache)
    nil))
                                        ;}}}


;;{{{ Syntax Checking and Miscellaneous function loading
;; erlang flycheck support and custom helpers
(require 'erlang-flycheck-config)
(require 'erlang-helper-config)
                                        ;}}}


(provide 'erlang-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; erlang-config.el ends here
