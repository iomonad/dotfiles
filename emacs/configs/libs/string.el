;******************************************************************************;
;                                                                              ;
;               string.el for string                                           ;
;               Created on : Thu Oct 20 15:36:57 2011                          ;
;               Made by : David "Thor" GIRON <thor@epitech.net>                ;
;                                                                              ;
;******************************************************************************;



(require 'list)



(defun string-reverse (s)
  "Returns the reversed string of 's'."
  (concat (reverse (string-to-list s)))
)

(defun string-length (s)
  "Returns the length of string 's'."
  (length s)
)

(defun string-fill (len)
  (make-string len ? )
)

(defun string-compare (s1 s2)
  "Compares 's1' and 's2'. Returns :
    - -1 if 's1' is shorter than 's2'
    - 0 if 's1' and 's2' are equal
    - 1 if 's1' is longer than 's2'"
  (let ((len1 (string-length s1))
	(len2 (string-length s2)))
    (cond
     ((= len1 len2) 0)
     ((<  len1 len2) -1)
     ((>  len1 len2) 1)
     )
    )
  )

(defun string-longest (s1 s2)
  "Returns the length of the longest string"
  (let ((res (string-compare s1 s2)))
    (cond
     ((= res 1 ) (string-length s1))
     ((= res 0 ) (string-length s1))
     ((= res -1) (string-length s2))
     )
    )
  )

(defun string-pick-longest (s1 s2)
  "Returns the longest string between 's1' and 's2', or 's1' otherwise."
  (let ((res (string-compare s1 s2)))
    (cond
     ((= res 1 ) s1)
     ((= res 0 ) s1)
     ((= res -1) s2)
     )
    )
  )

(defun string-longest-from-list (l)
  "Returns the length of longest string of the strings list 'l'."
  (list-fold
   (lambda (acc s) (if (> acc (string-length s)) acc (string-length s)))
   0
   l)
  )


(defun string-pick-longest-from-list (l)
  "Returns the longest string of the strings list 'l'."
  (list-fold
   (lambda (s1 s2) (string-pick-longest s1 s2))
   ""
   l)
  )



;******************************************************************************;
(provide 'string)
