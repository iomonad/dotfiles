;******************************************************************************;
;                                                                              ;
;               comments.el for automatic comments generation                  ;
;               Created on : Fri Oct 21 17:36:51 2011                          ;
;               Made by : David "Thor" GIRON <thor@epitech.net>                ;
;                                                                              ;
;******************************************************************************;



(require 'string)


(set 'line-std-width 80)



;******************************************************************************;
;                                                                              ;
;                          Comments tokens primitives                          ;
;                                                                              ;
;******************************************************************************;

(defun comments-start-token ()
  "Returns the comment start string of the current mode"
  comment-start
  )

(defun comments-end-token ()
  "Returns the comment end string of the current mode if any, or a
reversed string of the comment start otherwise."
  (if (/= (length comment-end) 0)
      comment-end
    (string-reverse comment-start)
    )
  )

(defun comments-start-token-length ()
  "Returns the length of the comment start string of the current mode."
  (string-length (comments-start-token))
)

(defun comments-end-token-length ()
  "Returns the length of the comment end string of the current mode."
  (string-length (comments-end-token))
)

(defun comments-tokens-length ()
  "Returns the total length of the comments tokens."
  (+ (comments-start-token-length) (comments-end-token-length))
)



;******************************************************************************;
;                                                                              ;
;                              Padding primitives                              ;
;                                                                              ;
;******************************************************************************;

(defun comments-compute-left-padding (s-text i-offset)
  "Returns a pair (lpad . rpad) for a left padded comments line."
  (let* ((i-len (+ (string-length s-text) i-offset))
	 (i-rpad (- line-std-width i-len (comments-tokens-length))))
    (cons 0 i-rpad)
    )
  )

(defun comments-compute-center-padding (s-text)
  "Returns a pair (lpad . rpad) for a center padded comments line."
  (let* ((i-len (string-length s-text))
	 (i-tpad (- line-std-width i-len (comments-tokens-length))))
    (if (= (% i-tpad 2) 0)
	(cons (/ i-tpad 2) (/ i-tpad 2))
      (cons (/ i-tpad 2) (+ (/ i-tpad 2) 1))
      )
    )
  )

(defun comments-compute-right-padding (s-text i-offset)
  "Returns a pair (lpad . rpad) for a right padded comments line."
  (let* ((i-len (+ (string-length s-text) i-offset))
	 (i-lpad (- line-std-width i-len (comments-tokens-length))))
    (cons i-lpad 0)
    )
  )



;******************************************************************************;
;                                                                              ;
;                              Contents producers                              ;
;                                                                              ;
;******************************************************************************;

(defun comments-make-padded-line (s-text i-lpad i-rpad)
  "Returns a comments string padded on line-std-width columns."
  (concat (comments-start-token)
	  (make-string i-lpad ? )
	  s-text
	  (make-string i-rpad ? )
	  (comments-end-token)
	  "\n"
	  )
  )

(defun comments-make-left-padded-line (s-text i-offset)
  "Returns a comments string left padded on line-std-width columns."  
  (let* ((pad (comments-compute-left-padding s-text i-offset)))
    (comments-make-padded-line
     (concat (make-string i-offset ? ) s-text)
     (car pad)
     (cdr pad))
    )
  )

(defun comments-make-center-padded-line (s-text)
  "Returns a comments string center padded on line-std-width columns."  
  (let* ((pad (comments-compute-center-padding s-text)))
    (comments-make-padded-line s-text (car pad) (cdr pad))
    )
  )

(defun comments-make-right-padded-line (s-text i-offset)
  "Returns a comments string right padded on line-std-width columns."  
  (let* ((pad (comments-compute-right-padding s-text i-offset)))
    (comments-make-padded-line
     (concat s-text (make-string i-offset ? ))
     (car pad)
     (cdr pad))
    )
  )

(defun comments-make-bar ()
  "Returns as a string a full comments bar of line-std-width."
  (concat (comments-start-token) 
	  (make-string (- line-std-width (comments-tokens-length)) ?*)
	  (comments-end-token) "\n")
  )

(defun comments-make-empty-line ()
  "Returns as a string an empty comments line of line-std-width."
  (concat (comments-start-token) 
	  (make-string (- line-std-width (comments-tokens-length)) ? )
	  (comments-end-token) "\n")
  )



;******************************************************************************;
;                                                                              ;
;                            Interactives functions                            ;
;                                                                              ;
;******************************************************************************;

(defun comments-insert-left-padded-line (s-text i-offset)
  "Inserts in the current buffer a comments string left padded on
   line-std-width columns."
  (interactive)
  (insert (comments-make-left-padded-line s-text i-offset))
  )

(defun comments-insert-center-padded-line (s-text)
  "Inserts in the current buffer a comments string center padded on
   line-std-width columns."
  (interactive)
  (insert (comments-make-center-padded-line s-text))
  )

(defun comments-insert-right-padded-line (s-text i-offset)
  "Inserts in the current buffer a comments string right padded on
   line-std-width columns."
  (interactive)
  (insert (comments-make-right-padded-line s-text i-offset))
  )

(defun comments-insert-bar ()
  "Inserts in the current buffer a line-std-width comments bar."
  (interactive)
  (insert (comments-make-bar))
  )

(defun comments-insert-empty-line ()
  "Inserts in the current buffer a line-std-width comments empty line."
  (interactive)
  (insert (comments-make-empty-line))
  )

(defun comments-insert-box (s-text)
  "Inserts a box of std-width with center padded 's-text'."
  (interactive "sBox content: ")
  (comments-insert-bar)
  (comments-insert-empty-line)
  (comments-insert-center-padded-line s-text)
  (comments-insert-empty-line)
  (comments-insert-bar)
  )

(defun comments-insert-small-box (s-text)
  "Inserts a small box of std-width with center padded 's-text'."
  (interactive "sBox content: ")
  (comments-insert-bar)
  (comments-insert-center-padded-line s-text)
  (comments-insert-bar)
  )



;******************************************************************************;
(provide 'comments)
