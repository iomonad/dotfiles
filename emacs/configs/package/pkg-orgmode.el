;; Filename: pkg-orgmode.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/16/2017 Sunday 21:31:53
;; Description: Org-Mode configuration

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :ensure t)

;; Default keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

;; Org file directory
(setq org-agenda-files '("~/.emacs.d/org/"))

;; Org Template
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/.emacs/org/todo.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))


;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))

;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))

;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))


(provide 'pkg-orgmode)
