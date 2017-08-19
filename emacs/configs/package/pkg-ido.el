;; Filename: pkg-ido.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/09/2017 Sunday 11:19:33
;; Description:  Improved movement

(use-package flx-ido :ensure t)
(use-package ido-hacks :ensure t)
(use-package ido-vertical-mode :ensure t)

(use-package ido :ensure t
  :init (setq ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always)
  :config (progn
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (ido-mode t)
            (ido-hacks-mode)
            (ido-vertical-mode)))

(provide 'pkg-ido)
