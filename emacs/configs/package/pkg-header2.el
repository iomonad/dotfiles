;; Filename: pkg-header2.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/18/2017 Tuesday 10:12:21
;; Description: Auto header configurations

;; -*- coding:utf-8; -*-

(use-package header2
  :defer t
  :ensure t
  :commands (auto-update-file-header auto-make-header)
  :init
  (progn
    (defconst auto-headers-hooks '(verilog-mode-hook
                                   emacs-lisp-mode-hook
                                   c-mode-hook
                                   c-mode-common-hook
                                   vlog-mode-hook
                                   python-mode-hook
                                   makefile-mode-hook
                                   sh-mode-hook
                                   cperl-mode-hook
                                   haskell-mode-hook
                                   java-mode-hook
                                   javascript-mode-hook
                                   clojure-mode-hook
                                   markdown-mode-hook
                                   scheme-mode-hook
                                   common-lisp-mode-hook
                                   c++-mode-hook
                                   text-mode-hook
                                   latex-mode-hook
                                   rust-mode-hook
                                   haskell-mode-hook
                                   clojure-mode-hook
                                   html-mode-hook
                                   css-mode-hook)
      "List of hooks of major modes in which headers (should )ould be auto-inserted.")
    
    (defun turn-on-auto-headers ()
      "Turn on auto headers only for specific modes."
      (interactive)
      (dolist (hook auto-headers-hooks)
        (add-hook hook #'auto-make-header)))
    
    (defun turn-off-auto-headers ()
      "Turn off auto headers only for specific modes."
      (interactive)
      (dolist (hook auto-headers-hooks)
        (remove-hook hook #'auto-make-header)))

    ;; Time formating for humans
    (setq header-date-format "%x %A %T")
    ;; Updated on exit
    (add-hook 'write-file-hooks     'auto-update-file-header)

    ;; User custom directives
    (setq header-author 'user-full-name)
    (setq header-file-name 'buffer-file-name)
    (setq header-creation-date 'current-time-string)
    (setq header-modification-author 'user-full-name)

    (defsubst ascii-header ()
      "Insert ascii logo in the header"
      (insert header-prefix-string "  .-.\n")
      (insert header-prefix-string (format " /'v'\\    (c) 2008-2017 %s\n" *user-name*))
      (insert header-prefix-string "(/ O \\)\n")
      (insert header-prefix-string (format "=='='==<     <%s>\n" *user-email*))
      (insert header-prefix-string "  |_|\n"))

    (defsubst copy-header ()
      "Serious header"
      (insert header-prefix-string (format "Copyright (c) 2008-2017 %s <%s>\n" *user-name* *user-email*)))
    
    (defsubst last-edit ()
      "Insert current user's name (`user-full-name') as this file's author."
      (insert header-prefix-string (format "Last edit by %s@%s" *short-name* (getenv "HOSTNAME") )))

    (setq header-copyright-notice "The sofware is provided under Mit License.\nFor the full copyright and license information, please view the LICENSE file.\n")
    ;; Composition
    (setq make-header-hook '(
                             header-file-name
                             copy-header
                             header-blank
                                        ;header-copyright
                                        ;header-blank
                             header-modification-date 
                             header-description
                             
                             ))
    (turn-on-auto-headers)
    ))

(provide 'pkg-header2)
