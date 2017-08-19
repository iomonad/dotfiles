;; Filename: conf-themes.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 05/28/2017 Sunday 15:57:05
;; Description: Custom paths for custom themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/community")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Autoload theme
(load-theme 'nord t)

(provide 'conf-themes)
