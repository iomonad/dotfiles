;******************************************************************************;
;                                                                              ;
;               list.el for list                                               ;
;               Created on : Thu Oct 20 10:02:03 2011                          ;
;               Made by : David "Thor" GIRON <thor@epitech.net>                ;
;                                                                              ;
;******************************************************************************;



(defun list-map (f l)
  "Map function 'f' over list 'l' and returns the result list."
  (mapcar f l
   )
  )

(defun list-iter (f l)
  "Map procedure 'f' over list 'l' and returns nil."
  (mapc f l)
  nil
  )

(defun list-fold (f s l)
  "Returns f (... (f (f s e1) e2) ...) en."
  (reduce f l :initial-value s)
  )



;******************************************************************************;
(provide 'list)
