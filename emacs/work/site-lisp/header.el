;*******************************************************************************;
;                                                                               ;
;                   42_header.el for 42 Emacs header                            ;
;                   Created on : Tue Jun 18 10:46:22 2013                       ;
;                   Made by : David "Thor" GIRON <thor@42.fr>                   ;
;                                                                               ;
;*******************************************************************************;



(require 'string)
(require 'list)
(require 'comments)



;******************************************************************************;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    filename_____________________________.ext          :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: login____ <mail_______@student.42.fr>      +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: yyyy/mm/dd 15:27:11 by login____         #+#    #+#              ;
;    Updated: yyyy/mm/dd 15:27:11 by login____        ###   ########.fr        ;
;                                                                              ;
;******************************************************************************;



(global-set-key (kbd "C-c C-h") 'header-insert)
(setq write-file-hooks (cons 'header-update write-file-hooks))


(set 'user-login (let ((login (getenv "USER")))
				   (if (string= login nil)
					   "marvin"
					 login)
				   )
	 )


(set 'user-mail (let ((mail (getenv "MAIL")))
				   (if (string= mail nil)
					   "marvin@42.fr"
					 mail)
				   )
	 )


(set 'left-std-margin 5)
(set 'right-std-margin 5)
(set 'info-std-width 41)


(set 'ft-1 "        :::      ::::::::")
(set 'ft-2 "      :+:      :+:    :+:")
(set 'ft-3 "    +:+ +:+         +:+  ")
(set 'ft-4 "  +#+  +:+       +#+     ")
(set 'ft-5 "+#+#+#+#+#+   +#+        ")
(set 'ft-6 "     #+#    #+#          ")
(set 'ft-7 "    ###   ########.fr    ")
(set 'ft-std-width 25)



;*******************************************************************************;


(defun header-chop-str (str n)
  (if (> (length str) n)
	  (let* ((max (- n 3))
	  		(new (substring str 0 max)))
	  	(concat new "..."))
	str)
  )

(defun header-make-left-margin ()
  "Creates the header comments start token and left margin"
  (let ((fill (string-fill (- left-std-margin (comments-start-token-length)))))
	(concat (comments-start-token) fill))
  )

(defun header-make-right-margin ()
  "Creates the header right margin and comments end token"
  (let ((fill (string-fill (- right-std-margin (comments-end-token-length)))))
	(concat fill (comments-end-token)))
  )

(defun header-make-central-gap (left-chunk right-chunk)
  "Creates the gap between the left infos block and the right logo"
  (string-fill (- line-std-width
				  (string-length left-chunk)
				  (string-length right-chunk)))
  )

(defun header-make-file-name ()
  "Creates the 'file.ext' entry of the header."
  (let* ((filename (header-chop-str (file-name-nondirectory (buffer-file-name))
									info-std-width))
		 (fill (string-fill (- info-std-width (string-length filename)))))
	(concat filename fill))
  )

(defun header-make-by ()
  "Creates the 'By: login <mail>' entry of the header."
  (let* ((mail-span (- info-std-width (+ (length user-login) 7)))
		 (by (concat "By: " user-login " <" (header-chop-str user-mail mail-span) ">"))
		 (fill (string-fill (- info-std-width (string-length by)))))
	(concat by fill))
  )

(defun header-make-creation-date ()
  "Creates the 'Created: yyyy/mm/dd hh:mm:ss' entry of the header.'"
  (concat "Created: " (format-time-string "%Y/%m/%d %T") " by " user-login)
  )

(defun header-make-update-date ()
  "Creates the 'Updated: yyyy/mm/dd hh:mm:ss' entry of the header.'"
  (concat "Updated: " (format-time-string "%Y/%m/%d %T") " by " user-login)
  )


;*******************************************************************************;



(defun header-insert-line-01 ()
  "Line 1 of the header"
  (comments-insert-bar)
  )

(defun header-insert-line-02 ()
  "Line 2 of the header"
  (comments-insert-empty-line)
  )

(defun header-insert-line-03 ()
  "Line 3 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (central-gap (header-make-central-gap
					   left-margin
					   (concat ft-1 right-margin))))
	(insert (concat left-margin central-gap ft-1 right-margin))
	)
  )

(defun header-insert-line-04 ()
  "Line 4 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (filename (header-make-file-name))
		 (central-gap (header-make-central-gap (concat left-margin filename)
											   (concat ft-2 right-margin))))
	(insert (concat left-margin filename central-gap ft-2 right-margin))
	)
  )

(defun header-insert-line-05 ()
  "Line 5 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (central-gap (header-make-central-gap left-margin
											   (concat ft-3 right-margin))))
	(insert (concat left-margin central-gap ft-3 right-margin))
	)
  )

(defun header-insert-line-06 ()
  "Line 6 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (by (header-make-by))
		 (central-gap (header-make-central-gap (concat left-margin by)
											   (concat ft-4 right-margin))))
	(insert (concat left-margin by central-gap ft-4 right-margin))
	)
  )

(defun header-insert-line-07 ()
  "Line 7 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (central-gap (header-make-central-gap left-margin
											   (concat ft-5 right-margin))))
	(insert (concat left-margin central-gap ft-5 right-margin))
	)
  )

(defun header-insert-line-08 ()
  "Line 8 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (created (header-make-creation-date))
		 (central-gap (header-make-central-gap (concat left-margin created)
											   (concat ft-6 right-margin))))
	(insert (concat left-margin created central-gap ft-6 right-margin))
	)
  )

(defun header-insert-line-09 ()
  "Line 9 of the header"
  (let* ((left-margin (header-make-left-margin))
		 (right-margin (header-make-right-margin))
		 (updated (header-make-update-date))
		 (central-gap (header-make-central-gap (concat left-margin updated)
											   (concat ft-7 right-margin))))
	(insert (concat left-margin updated central-gap ft-7 right-margin))
	)
  )

(defun header-insert-line-10 ()
  "Line 10 of the header"
  (comments-insert-empty-line)
  )

(defun header-insert-line-11 ()
  "Line 11 of the header"
  (comments-insert-bar)
  )



;*******************************************************************************;



(defun header-insert ()
  "Creates a header for the current source file."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(header-insert-line-01)
	(header-insert-line-02)
	(header-insert-line-03) (newline)
	(header-insert-line-04) (newline)
	(header-insert-line-05) (newline)
	(header-insert-line-06) (newline)
	(header-insert-line-07) (newline)
	(header-insert-line-08) (newline)
	(header-insert-line-09) (newline)
	(header-insert-line-10)
	(header-insert-line-11)
	)
  )


(defun header-update ()
  "Updates the header for the current source file."
  (interactive)
  (save-excursion
    (if (buffer-modified-p)
        (progn
          (goto-char (point-min))
          (if (search-forward "Updated" nil t)
              (progn
                (delete-region
                 (progn (beginning-of-line) (point))
                 (progn (end-of-line) (point)))
				(header-insert-line-09)
                (message "Header up to date."))))))
  nil)



;******************************************************************************;
(provide 'header)
