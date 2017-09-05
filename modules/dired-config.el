;;; package  --- dired-config.el
;;;
;;; Commentary:
;;;
;;; Code:
;;;
;;; Filename   : dired-config.el
;;; Description: file explorer operations through dired
;;;============================================================================
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t))

;;-----------------------------------------------------------------------------
;; http://www.emacswiki.org/DiredPlus
;;-----------------------------------------------------------------------------
(use-package dired+
  :ensure t
  ;; :defer t
  :config
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil))

;; http://truongtx.me/2013/04/24/dired-as-default-file-manager-1-introduction

(use-package dired-details+
  :ensure t
  :defer t)

;;-----------------------------------------------------------------------------
;;  Ibuffer like filtering for dired
;;-----------------------------------------------------------------------------
(use-package dired-filter
  :ensure t
  :init
   (setq dired-filter-show-filters 'nil))

;;---------------------------------------------------------------------------
;; for handling dired collection
;;---------------------------------------------------------------------------
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies  'always)
;; directory. This  means: if there  is a  dired buffer displayed  in the
;; next window, use its current subdir,  instead of the current subdir of
;; this dired  buffer. The target  is used in  the prompt for  file copy,
;; rename etc.
(setq dired-dwim-target t)

; (setq font-lock-maximum-decoration (quote ((dired-mode) (t . t))))

;; Dired listing switches
;;  -a : Do not ignore entries starting with .
;;  -l : Use long listing format.
;;  -G : Do not print group names like 'users'
;;  -h : Human-readable sizes like 1K, 234M, ..
;;  -v : Do natural sort .. so the file names starting with . will show up first.
;;  -F : Classify filenames by appending '*' to executables,
;;       '/' to directories, etc.
;; (setq dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"
(setq dired-listing-switches "-ahlv")

;;-----------------------------------------------------------------------------
;; dired rainbow
;;-----------------------------------------------------------------------------
(defconst dired-audio-files-extensions
  '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
  "Dired Audio files extensions")

(defconst dired-video-files-extensions
    '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
      "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
    "Dired Video files extensions")

(use-package dired-rainbow
  :defer t
  :config
  (progn
    (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
    (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))

    (dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub"))
    (dired-rainbow-define excel "#3465a4" ("xlsx"))

    (dired-rainbow-define log "#c17d11" (".*\\.log"))
    (dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "h" "java" "pl" "rb" "R" "php"))

    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
    (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

    (dired-rainbow-define-chmod executable-unix "Green" "-[rw-]+x.*")

    (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))
    (dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)
    (dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)))

;;-----------------------------------------------------------------------------
;; addons for dired
;;-----------------------------------------------------------------------------
(use-package find-dired
   :ensure t
   :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))

;;-----------------------------------------------------------------------------
;; peep at files in another window from dired buffers
;;-----------------------------------------------------------------------------
(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :init
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-enable-on-directories t)
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(message "loaded dired-config.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-config)
;;; dired-config.el ends here
