;; Filename: conf-erc.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:35:55
;; Description: Configuration for the emacs irc client

(use-package erc :ensure t
	     :init (progn
		     (defvar erc-insert-post-hook)
		     (setq erc-nick "iomonad-work"
			   erc-fill-column (- (window-width) 2)
			   erc-input-line-position -2
			   erc-log-insert-log-on-open nil
			   erc-log-channels t
			   erc-log-channels-directory (concat files-dir "erc")
			   erc-save-buffer-on-part t
			   erc-hide-timestamps nil
			   erc-hide-list '("JOIN" "PART" "QUIT")
			   erc-max-buffer-size 20000
			   erc-truncate-buffer-on-save t
			   erc-keywords '("iomonad-work")
			   erc-timestamp-only-if-changed-flag nil
			   erc-timestamp-format "[%R] "
			   erc-insert-timestamp-function 'erc-insert-timestamp-left
			   erc-server-coding-system '(utf-8 . utf-8)
			   erc-interpret-mirc-color t
			   erc-kill-buffer-on-part t
			   erc-kill-queries-on-quit t
			   erc-kill-server-buffer-on-quit t
			   erc-autojoin-channels-alist '(("freenode.net" "#gentoo" "#emacs"))))
	     :config (progn
		       (erc-netsplit-mode 1)
		       (erc-match-mode 1)
		       (add-hook 'erc-insert-post-hook
				 #'erc-truncate-buffer)
		       (add-hook 'erc-mode-hook
				 #'(lambda ()
				     (setq show-trailing-whitespace nil)
				     (auto-fill-mode 0)))
		       (add-hook 'erc-after-connect
				 #'(lambda (SERVER NICK)
				     (erc-message "PRIVMSG" (format "NickServ identify %s" (read-passwd "IRC NickServ Password: ")))))))

(defun erc-connect ()
  "Connect to ERC."
  (interactive)
  (erc :server "irc.freenode.net" :port 6666 :nick "iomonad-work" :full-name "Clement Trösa"))
(provide 'conf-erc)
