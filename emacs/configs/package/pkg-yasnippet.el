;; Filename: pkg-yasnippet.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:41:16
;; Description: Snippets to improve completion

(use-package yasnippet :ensure t
  :functions yas-global-mode
  :diminish yas-minor-mode
  :defer 5)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/community"  ;; Community Snippets
        "~/.emacs.d/snippets/custom"     ;; User custom snippets
        ))

(yas-global-mode 1)

(provide 'pkg-yasnippet)
