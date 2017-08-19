;; Filename: lang-perl.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/24/2017 Monday 10:42:12
;; Description:  Perl Language

;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)

;; just spaces
(setq-default indent-tabs-mode nil)

;; Use 4 space indents via cperl mode
(custom-set-variables
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t)
 )

(provide 'lang-perl)
