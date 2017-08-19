;; Filename: conf-repos.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:34:50
;; Description: Set repository to retrive packages

(require 'package)

(setq package-user-dir  (expand-file-name (convert-standard-filename "packages") user-emacs-directory)
      package-enable-at-startup nil
      package-archives '(("melpa"        . "http://melpa.org/packages/")
			 ("gnu"          . "http://elpa.gnu.org/packages/")
			 ("marmalade"    . "http://marmalade-repo.org/packages/")
			 ("org"          . "http://orgmode.org/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'conf-repos)
