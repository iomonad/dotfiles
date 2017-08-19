;; Filename: pkg-agressive-indent.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/25/2017 Tuesday 21:18:03
;; Description: Indent made agressive
;; Extra: electric-indent-mode is enough to keep your code nicely aligned when all you do is type.
;;        However, once you start shifting blocks around, transposing lines, or slurping and barfing sexps,
;;        indentation is bound to go wrong.

(use-package aggressive-indent
  :ensure t)

(global-aggressive-indent-mode 1) ; Oh yeah
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'scala-mode)

(provide 'pkg-agressive-indent)
