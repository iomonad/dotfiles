;; Filename: lang-web.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 09:07:35
;; Description: Web developpement related configurations

(use-package htmlize :ensure t)

(use-package js2-mode :ensure t)

(use-package json-mode :ensure t)

(use-package rainbow-mode :ensure t
  :diminish rainbow-mode)

(use-package restclient :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

(provide 'lang-web)
