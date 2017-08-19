;; Filename: conf-recentf.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:34:20
;; Description: Store recent edited file in log file

(require 'recentf)

(recentf-mode 1)

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50)

(add-to-list 'recentf-exclude *config-dir*)
(add-to-list 'recentf-exclude config-core-dir)
(add-to-list 'recentf-exclude config-pkg-dir)
(add-to-list 'recentf-exclude config-langs-dir)
(add-to-list 'recentf-exclude config-exp-dir)
(add-to-list 'recentf-exclude *defun-dir*)

(provide 'conf-recentf)
