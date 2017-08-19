;; Filename: conf-bookmarks.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/20/2017 Thursday 10:22:38
;; Description:  Bookmarked path for fast moving

(require 'bookmark)

(setq bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Project"       (filename . "~/projects"))
                       ("Sandbox"       (filename . "~/.emacs.d/sandbox"))
                       ("Org-Mode-Path" (filename . "~/.emacs.d/org"))
                       ("Emacs.d"    (filename . "~/.emacs.d"))))

(provide 'conf-bookmarks)
