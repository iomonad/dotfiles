;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../macros.lisp             ;;
;;-----------------------------------------

;; create given groups while keeping focus on current.
(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
  `(run-commands ,@ns)))

;; faster hook management.
(defmacro replace-hook (hook fn)
  `(remove-hook, hook, fn)
  `(add-hook, hook, fn))

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.

;; (define-frame-preference "WORKSPACE#"
;;   (frame(number) | raise(boolean) | lock(boolean) [lock AND raise == jumpto]))

(define-frame-preference "Ⅲ"
  (0    nil   t     :class    "dwb"))

(define-frame-preference "ⅠV"
  (0    nil   t     :class    "Firefox")
  (0    nil   t     :instance "Navigator"))

(define-frame-preference "VⅠ"
  (0    t     t     :class "pavucontrol"))

;; EOF
