;;; package  --- neotree-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename: neotree-config.el
;;; Description: Emacs tree plugin like NerdTree for Vim
;;;              configuration file for neotree custom settings.
;;;
;;; elisp code for customizing the neotree settings
;;;===========================================================================
(require 'neotree)
(require 'all-the-icons) ; collect various Icon Fonts and propertize them within Emacs
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a tree plugin like NerdTree for Vim (themes and modes)                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq neo-theme (if window-system 'icons 'arrow))
;(setq neo-theme 'ascii)
(custom-set-faces
 '(neo-banner-face ((t . (:inherit shadow))) t)
 '(neo-header-face ((t . (:inherit shadow))) t)
 '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
 '(neo-dir-link-face ((t . (:inherit dired-directory))) t)
 '(neo-file-link-face ((t . (:inherit default))) t)
 '(neo-button-face ((t . (:inherit dired-directory))) t)
 '(neo-expand-btn-face ((t . (:inherit button))) t))


(provide 'neotree-config)

;;; neotree-config.el ends here
