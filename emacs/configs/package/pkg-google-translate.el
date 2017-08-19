;; Filename: pkg-google-translate.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/09/2017 Sunday 11:12:47
;; Description:  Translate utils using google api

(use-package google-translate
  :ensure t)


(setq google-translate-translation-directions-alist
      '(("fr" . "en")
        ("en" . "fr")
        ("nl" . "fr")
        ("fr" . "nl")
        ("fr" . "se")
        ("se" . "fr")))

(setq google-translate-default-source-language "fr")
(setq google-translate-default-target-language "en")
(setq google-translate-enable-ido-completion t)
(provide 'pkg-google-translate)
