
(define-frame-preference "web"
                         (0 t t :class "Firefox"))

(define-frame-preference "tex"
                         (0 t t :class "Lyx"))

(define-frame-preference "gimp"
                         (0 t t :class "Gimp"))

(setf *window-type-override-list*
      '((:dialog :instance "bashrun2-run-dialog")))
