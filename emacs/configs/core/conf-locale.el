;; Filename: conf-locale.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:32:51
;; Description: Set locale for emacs

(defun set-locale (locale)
  "Set locale to LOCALE."
  (interactive)
  (set-charset-priority 'unicode)
  (setq locale-coding-system locale)
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))
(set-locale 'utf-8)

(provide 'conf-locale)
