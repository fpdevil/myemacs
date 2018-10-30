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
(require 'cl-lib)


;; {{{ utility function to handle versioned dirs
(defun wild (dir stem)
  "Return the last (alphabetically) filename that match DIR/STEM*."
  (car
   (reverse
    (sort
     (let (value)
       (dolist (element (file-name-all-completions stem dir) value)
         (setq value (cons (concat dir element) value)))) 'string-lessp))))
;;}}}


;;** [Setup] - locations for erlang root and other packages
(defvar erlang-erl-path "/usr/local/opt/erlang/lib/erlang")
(setq erlang-root-dir erlang-erl-path)

(defun my-erlang-load-hook ()
  "Erlang root directory bootstrap."
  (setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang"))
(add-hook 'erlang-mode-hook 'my-erlang-load-hook)

;;find and load the erlang mode .el file
(defvar erlang-erlmode-path
  (concat (wild (concat erlang-erl-path "/lib/") "tools-") "emacs"))
(add-to-list 'load-path erlang-erlmode-path)           ; add erlang tools to the path
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

;;** now start the erlang mode
(require 'erlang-start)


;;** [associate file extensions with major-modes]
(setq auto-mode-alist
      (reverse
       (append auto-mode-alist
               '(("\\.rel$"          . erlang-mode)
                 ("\\.app$"          . erlang-mode)
                 ("\\.appSrc$"       . erlang-mode)
                 ("\\.app.src$"      . erlang-mode)
                 ("\\.hrl$"          . erlang-mode)
                 ("\\.erl$"          . erlang-mode)
                 ("\\.yrl$"          . erlang-mode)
                 (".yaws?'"          . erlang-mode)
                 (".*\\.script\\'"   . erlang-mode)
                 (".*\\.escript\\'"  . erlang-mode)
                 (".escript?'"       . erlang-mode)
                 ("rebar.config$"    . erlang-mode)
                 ("relx.config$"     . erlang-mode)
                 ("sys.config$"      . erlang-mode)))))


;;** [WRANGLER] - erlang path for Wrangler and start Wrangler, load the graphviz
;; (defvar erlang-wrangler-path "/usr/local/lib/erlang/lib/wrangler-1.2.0/elisp")
;; (unless (member erlang-wrangler-path load-path)
;;   (add-to-list 'load-path erlang-wrangler-path))
;; GraphViz from Wrangler package
;; (add-hook 'erlang-mode-hook 'erlang-wrangler-on)
;; some wrangler functionalities generate a .dot file and in order
;; to compile the same and view in graphviz specify the below.
;; (load-file (concat erlang-wrangler-path "/graphviz-dot-mode.el"))


;;** [MAN PAGES]
(setq erlang-man-root-dir (expand-file-name "man" erlang-erl-path))
(setq erlang-man-dirs (list '("Man - Commands" "lib/erlang/man/man1" t)
                            '("Man - Modules" "lib/erlang/man/man3" t)
                            '("Man - Files" "lib/erlang/man/man4" t)
                            '("Man - Applications" "lib/erlang/man/man6" t)))


;;** [imenu, indentation] - Imenu, indentation and machine-options
(defun init-erl-utils ()
  "interactive"
  (setq indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq erlang-indent-level 4)
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (imenu-add-to-menubar "imenu"))
(add-hook 'erlang-mode-hook 'init-erl-utils)


;;** [DISTEL] - Erlang Development Support
(defvar erlang-distel-path "/opt/erlang/distel/elisp")
(unless (member erlang-distel-path load-path)
  ;; add distel to end of load path
  (setq load-path (append load-path (list erlang-distel-path))))


;;** Erlang Distel setup continuation
(require 'distel)
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq distel-modeline-node "distel")
            ;; When starting an Erlang shell in Emacs, default in
            ;; the node name.  Pass -name, not -sname so you
            ;; could talk to this node from other machines.
            ;; (setq inferior-erlang-machine-options '("-name" "emacs"))
            (setq inferior-erlang-machine-options '("-sname" "emacs"))
            ;; tell distel to default to that node
            (setq erl-nodename-cache
                  (make-symbol
                   (concat
                    "emacs@"
                    ;; Mac OS X uses "name.local" instead of "name", this should work
                    ;; pretty much anywhere without having to muck with NetInfo
                    ;; ... but I only tested it on Mac OS X.
                    (car (split-string (shell-command-to-string "hostname"))))))
            (setq erlang-compile-extra-opts (list 'debug_info))
            ;; when loading a beam file from emacs, add the path to erlang
            (setq erl-reload-dwim t)))

(add-hook 'erlang-mode-hook 'init-shell)

(distel-setup)

;;** [Erl Shell] - Setup the Erlang Shell
(defun init-shell ()
  (add-hook 'erlang-shell-mode-hook 'erlang-shell-init)
  (defun erlang-shell-init ()
    (setq comint-dynamic-complete-functions
          '(erl-nodename-complete  comint-replace-by-expanded-history)))
  (defun erl-nodename-complete ()
    "Call erl-complete if we have an Erlang node name"
    (if erl-nodename-cache
        (erl-complete erl-nodename-cache)
      nil))
  ;; A number of the erlang-extended-mode key bindings are useful in the shell too
  ;; setup shortcut keys
  (defconst distel-shell-keys
    '(("\C-\M-i"   erl-complete)
      ("\M-?"      erl-complete)
      ("\M-."      erl-find-source-under-point)
      ("\M-,"      erl-find-source-unwind)
      ("\M-*"      erl-find-source-unwind)
      )
    "Additional keys to bind when in Erlang shell.")

  (add-hook 'erlang-shell-mode-hook
            (lambda ()
              ;; Comint binding that conflicts with some of the
              ;; distel keybindings.
              (define-key erlang-shell-mode-map (kbd "C-c C-d") nil)
              ;; add some Distel bindings to the Erlang shell
              (dolist (spec distel-shell-keys)
                (define-key erlang-shell-mode-map (car spec) (cadr spec))))))


;;** [auto-complete-mode]
;;    auto-complete-mode so can interact with inferior erlang and
;;    popup completion turn on when needed.
(add-hook 'erlang-mode-hook
          (lambda () (auto-complete-mode 1)))

;; -- setup auto complete for distel
;;    erlang ide set-up and erlang auto-completion using auto-complete and distel
(after 'auto-complete
  (require 'auto-complete-distel)
  ;;(setq ac-modes (append ac-modes '(erlang-mode)))
  ;;(setq ac-modes (append ac-modes '(erlang-shell-mode)))
  (setq ac-modes (append ac-modes (list 'erlang-mode)))
  (setq ac-modes (append ac-modes (list 'erlang-shell-mode)))
  (add-to-list 'ac-sources 'auto-complete-distel))


;;** [company-mode]
;;    setup company completions with distel
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


;;** [ESENSE] - Erlang IDE Features
(defvar erlang-esense-path (wild "/opt/erlang/" "esense-"))
(defun init-esense ()
  "Set esense configuration."
  (add-to-list 'load-path erlang-esense-path)
  (require 'esense-start)
  (setq esense-indexer-program (concat erlang-esense-path "esense.sh"))
  (setq esense-setup-otp-search-directories t)
  (setq esense-module-search-directories (concat erlang-root-dir "/lib/*/src"))
  (setq esense-include-search-directories (concat erlang-root-dir "/lib/*/include"))
  (setq esense-distel-node "distel@apple"))

;;** initialize Esense
(init-esense)


;;** [Compile] - Erlang Make and Compile
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun my-erlang-mode-hook ()
  "Erlang hook for Make and Compile."
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

;;** [Comint] - Initialize comint mode
(add-hook 'comint-mode-hook 'my-comint)
(defun my-comint ()
  "Try to make the shell more like the real shell."
  (local-set-key [tab] 'comint-dynamic-complete)
  (local-set-key [(control up)] 'previous-line)
  (local-set-key [(control down)] 'next-line)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input))



;;** [IDE with EDTS]
;;    EDTS - Erlang Development Tool Suite
;;    start edts - place the configuration file .edts in the project
;;(add-to-list 'safe-local-variable-values '(erlang-indent-level . 4))
(defun start-edts ()
  "Initialize EDTS for ERLANG."
  (interactive)
  ;; Set the manual directory and indent level
  (setq edts-man-root "/usr/local/opt/erlang/lib/erlang")
  ;;(setq edts-data-directory (expand-file-name "edts" cache-dir))
  (setq edts-code-issue-wrap-around t)
  (setq edts-log-level 'debug)
  (require 'edts-start))


;; (setq edts-projects
;;      '(((name          . "code-dev")
;;         (node-sname    . "code")
;;         (root          . "~/code")
;;         ;; (otp-path      . "/opt/install/R14b03")
;;         (lib-dirs      . ("lib" "test")))))

;; EDTS uses the directory name for the name of its worker node, which often
;; collides with the name you’d use when running the application in an Erlang
;; node… The below setup adds “edts-” to the beginning of the node that EDTS
;; starts when opening the source code.
(defun custom-edts-project-config-default (config)
  (let ((entry (assq :node-sname config)))
    (setf (cdr entry) (concat "edts-" (cdr entry)))
    config))

(with-eval-after-load "edts-project"
  (advice-add 'edts-project--config-default :filter-return
              'custom-edts-project-config-default))

(add-hook 'erlang-mode-hook 'start-edts)
;;(add-hook 'after-init-hook 'start-edts)
;;(add-hook 'erlang-mode-hook (lambda () (require 'edts-start)))


;;** [Syntax Checking] - Syntax Checking and Miscellaneous function loading
;;                       erlang flycheck support and custom helpers
(require 'erlang-flycheck-config)
(require 'erlang-helper-config)


(provide 'erlang-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; erlang-config.el ends here
