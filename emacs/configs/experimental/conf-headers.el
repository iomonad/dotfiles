;; Filename: conf-headers.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:36:56
;; Description: Make autoheader mode active using built-in functions

(require 'autoinsert)

(auto-insert-mode 1)
(auto-insert)

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n"
         "# -*- mode: ruby; -*-\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n")
        ((python-mode . "Python program") nil
         "#!/usr/bin/env python\n"
         "# -*- mode: python; -*-\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n")
        ((lisp-mode . "Lisp program") nil
         ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
        ((emacs-lisp-mode . "Emacs lisp program") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
         ";;; Commentary:\n\n"
         ";;; Code:\n\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")
        ((c-mode . "C program") nil
         "/*\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Time-stamp: <>\n"
         " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         " * Description: " _ "\n"
         " */\n\n")
        ((sh-mode . "Shell script") nil
         "#!/usr/bin/env bash\n"
         "# -*- mode: sh; -*-\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n"
         "set -o errexit\n\n"
         "[ -z $BASH ] && (echo \"Not in a BASH sub process\"; exit 1)\n"
         "BASE_DIR=$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)\n\n")
        ((restclient-mode . "REST client") nil
         "# -*- restclient -*-\n\n")
        ((org-mode . "Org mode") nil
         "#+AUTHOR: " (user-full-name) "\n"
         "#+DATE: " (current-time-string) "\n"
         "#+STARTUP: showall\n\n")))

(provide 'conf-headers)
