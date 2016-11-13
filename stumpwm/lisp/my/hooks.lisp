(defmacro replace-hook (hook fn)
  `(remove-hook ,hook ,fn)
  `(add-hook ,hook ,fn))

(replace-hook *focus-group-hook* 'callback-groupchange)
(replace-hook *key-press-hook* 'show-key-seq)

(defun callback-groupchange (new-group old-group)
  (with-open-file (file (format nil "~A/log/stumpwm.log" (getenv "XDG_DATA_HOME"))
                        :direction         :output
                        :if-exists         :append
                        :if-does-not-exist :create)
    (format file "Workspace change: ~A to ~A~%" (group-name old-group) (group-name new-group))))
                         
(defun show-key-seq (key seq val)
  (message (print-key-seq (reverse seq))))
