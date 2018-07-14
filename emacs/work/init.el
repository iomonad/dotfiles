;******************************************************************************;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: iomonad <iomonad@riseup.net>               +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/07/14 10:14:47 by iomonad           #+#    #+#              ;
;    Updated: 2018/07/14 11:18:52 by iomonad          ###   ########.fr        ;
;                                                                              ;
;******************************************************************************;

(package-initialize)

(setq config_files "~/.emacs.d/site-lisp")
(setq load-path (append (list nil config_files) load-path))

(load "list.el")
(load "string.el")
(load "comments.el")
(load "header.el")

;; (autoload 'php-mode "php-mode" "Major mode for editing PHP code" t)
;; (add-to-list 'auto-mode-alist '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
; Set default emacs configuration
(set-language-environment "UTF-8")
;; (setq-default font-lock-global-modes nil)
(setq-default line-number-mode nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq-default c-backspace-function 'backward-delete-char)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                             64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
;;Load user configuration
(if (file-exists-p "~/.emacs.d/myemacs.el") (load-file "~/.emacs.d/myemacs.el"))
