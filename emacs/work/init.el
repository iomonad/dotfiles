;******************************************************************************;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: iomonad <iomonad@riseup.net>               +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/07/14 10:14:47 by iomonad           #+#    #+#              ;
;    Updated: 2019/06/11 11:57:54 by iomonad          ###   ########.fr        ;
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
	("59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "450f3382907de50be905ae8a242ecede05ea9b858a8ed3cc8d1fbdf2d57090af" "0d456bc74e0ffa4bf5b69b0b54dac5104512c324199e96fc9f3a1db10dfa31f3" default)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
	(cyberpunk-theme tuareg request go-autocomplete go-mode distinguished-theme magit markdown-mode yasnippet-snippets ensime cider multiple-cursors company use-package)))
 '(safe-local-variable-values
   (quote
	((company-clang-arguments "-I/Users/ctrouill/Projects/ft_malloc/includes" "-I/Users/ctrouill/Projects/ft_malloc/libft/includes" "~/Projects/malloc/includes" "~/Projects/malloc/libft/includes")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
