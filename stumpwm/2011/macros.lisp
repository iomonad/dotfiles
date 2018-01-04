(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
  `(run-commands ,@ns)))

(defmacro replace-hook (hook fn)
  `(remove-hook, hook, fn)
  `(add-hook, hook, fn))



(define-frame-preference "Ⅲ"
  (0    nil   t     :class    "dwb"))

(define-frame-preference "ⅠV"
  (0    nil   t     :class    "Firefox")
  (0    nil   t     :instance "Navigator"))

(define-frame-preference "VⅠ"
  (0    t     t     :class "pavucontrol"))

