
#|
(defun local-list (from-frame to-frame)
  (run-commands "echo-frame-windows"))
add-hook *focus-frame-hook* 'local-list)
|#

#|
(defun key-press-hook (key key-seq cmd)
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :top-right))
      (message "Key sequence: ~A" (print-key-seq (reverse key-seq))))
    (when (stringp cmd)
      (sleep 0.1))))
(replace-hook *key-press-hook* 'key-press-hook)
|#

