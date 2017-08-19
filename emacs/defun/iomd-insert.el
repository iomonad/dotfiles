;; Filename: iomd-insert.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/03/2017 Monday 11:42:46
;; Description: Some utilities to insert infromation in the buffer

(defun iomd:sign ()
  (interactive)
  (insert (concat " --" *short-name* "@"
                  (format-time-string "%Y-%m-%d"))))
(global-set-key "\C-c[" 'iomd:sign)

(defun iomd:insert-name-email ()
  (interactive)
  (insert (concat *user-name* " <" *user-email* ">")))
(global-set-key "\C-ce" 'iomd:insert-name-email)

(defun iomd:insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
  two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          (system-time-locale "fr_FR"))
      (insert (format-time-string format))))
(global-set-key "\C-cd" 'iomonad:insert-date)

(provide 'iomd-insert)
