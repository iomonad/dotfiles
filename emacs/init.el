;; Filename: init.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 05/28/2017 Sunday 15:50:34
;; Description: Entrypoint of the emacs configuration 


;; Folder variables

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar *config-dir* (expand-file-name (convert-standard-filename "configs") user-emacs-directory)) ;; Global Config Folder
(defvar config-core-dir (concat (file-name-as-directory *config-dir*) "core")) ;; Core feature of emacs
(defvar config-pkg-dir (concat (file-name-as-directory *config-dir*) "package")) ;; External packages configuration
(defvar config-langs-dir (concat (file-name-as-directory *config-dir*) "languages")) ;; Custom configurations for languages
(defvar config-libs-dir (concat (file-name-as-directory *config-dir*) "libs")) ;; External libraries go here
(defvar config-exp-dir (concat (file-name-as-directory *config-dir*) "experimental")) ;; Experimental features
(defvar *defun-dir* (expand-file-name (convert-standard-filename "defun") user-emacs-directory)) ;; Functions are stocked here
(defvar *plugins-dir* (expand-file-name (convert-standard-filename "plugins") user-emacs-directory)) ;; External plugins are here

(defun load-config-folders (config-folders)
  "Load emacs files in several paths"
  (if (file-exists-p config-folders)
      (progn
	(when init-file-debug
	  (require 'benchmark)) ;; Use benchmark library
	(add-to-list 'load-path config-folders)
	(mapc (lambda (fname) ;; Parse each features
		(let ((feat (intern (file-name-base fname)) ))
		  (if init-file-debug 
		      (message "Feature'%s' wijze in %.2fs" feat (benchmark-elapse (require feat fname)))
		    (require feat fname)))) ;; Import configurations
	      (directory-files config-folders t "\\.el")))
    (message "Directory \"%s\" niet gevonden. Geen extensies zijn geladen." config-folders)))

;; Load selected folders
(load-config-folders config-core-dir)
(load-config-folders config-langs-dir)
(load-config-folders config-pkg-dir)
(load-config-folders *defun-dir*)
(load-config-folders config-libs-dir)
(load-config-folders *plugins-dir*)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t)
 '(custom-safe-themes
   (quote
    ("8e0c6a96a17a5b45979c31265821053aff9beea9fb5ac5e41130e0c27a89214e" default)))
 '(tabbar-separator (quote (0.5))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
