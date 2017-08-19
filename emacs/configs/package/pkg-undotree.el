;; Filename: pkg-undotree.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:40:40
;; Description: Improved undo support and ascii modification tree

(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :init nil)
(defalias 'redo 'undo-tree-redo)

(provide 'pkg-undotree)
