;; Filename: lang-haskell.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/13/2017 Thursday 21:06:01
;; Description: Haskell language support

(use-package intero
  :ensure t)

(add-hook 'haskell-mode-hook 'intero-mode)


(provide 'lang-haskell)
