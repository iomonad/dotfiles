;; Filename: lang-text.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 09:13:03
;; Description: Text mode configuration

;(use-package flycheck :ensure t
;  :config (global-flycheck-mode t))

(use-package org :ensure t
  :init (progn
          (setq org-hide-leading-stars t
                org-hide-emphasis-markers t
                org-fontify-done-headline t
                org-src-fontify-natively t)))

(use-package markdown-mode :ensure t)

(use-package yaml-mode :ensure t)

(defun hook-text-mode ()
  "Hook  for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

(provide 'lang-text)
