;; Filename: lang-scala.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/25/2017 Tuesday 21:28:47
;; Description: Scala for the win

                                        ;(use-package ensime
                                        ;:ensure t)

(use-package sbt-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

(setq scala-indent:step 4)

(provide 'lang-scala)
