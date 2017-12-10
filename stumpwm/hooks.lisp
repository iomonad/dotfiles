;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../hooks.lisp              ;;
;;-----------------------------------------

#|
;; show local windows in frame when focusing on it.
;; unfortunately the echo command is cropped when
;; focused frame overlaps part of it's output.
(defun local-list (from-frame to-frame)
  (run-commands "echo-frame-windows"))
add-hook *focus-frame-hook* 'local-list)
|#

#|
;; display the keysequence in progress (found this).
(defun key-press-hook (key key-seq cmd)
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :top-right))
      (message "Key sequence: ~A" (print-key-seq (reverse key-seq))))
    (when (stringp cmd)
      (sleep 0.1))))
(replace-hook *key-press-hook* 'key-press-hook)
|#

;; EOF
