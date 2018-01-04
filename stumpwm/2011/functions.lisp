
(defun fmt-group-status (group)
  (let ((screen (group-screen group)))
    (cond ((eq group (screen-current-group screen))
           #\*)
          ((and (typep (second (screen-groups screen)) 'group)
                (eq group (second (screen-groups screen))))
           #\+)
          (t #\-))))

(defun move-window-to-next-group (current list)
"Move current window to next group but keep focus on current frame."
  (let ((next (next-group current (non-hidden-groups list)))
        (win (group-current-window current)))
    (when (and next win) (move-window-to-group win next))))

(defun exchange-windows-remain (win1 win2)
"Exchange windows but keep focus on current frame, unlike exchange-windows."
  (let ((f1 (window-frame win1))
        (f2 (window-frame win2)))
    (unless (eq f1 f2)
      (pull-window win1 f2)
      (pull-window win2 f1))))

(defun shift-windows-forward (frames win)
"Exchange windows through cycling frames."
  (when frames
          (let ((frame (car frames)))
                  (shift-windows-forward (cdr frames)
                                         (frame-window frame))
                  (when win
                           (pull-window win frame)))))

(defun remember-group (&optional (group (current-group))) ()
"Remember current group information before calling another command or
function. Combined with 'undo' command this allows for toggling between
the two undo states."
  (if (ensure-directories-exist *undo-data-dir*)
    (when group
      (dump-group-to-file
        (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"
        (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group))))
        :type "lisp" :defaults *undo-data-dir*)))))

(defun remember-all () ()
"Similiar to remember-group except all information is dumped, useful
for next startup or recalling all undo actions."
  (dump-to-datadir "rules") (dump-to-datadir "desktop"))

(defun select-random-bg-image ()
"Select a random image from *background-image-path* and display it
on the root window. This is a rewrite of another function to check
for errors and allow more than one picture type, as display command
will only display valid files anyway."
  (if (ensure-directories-exist *background-image-path*)
    (let ((file-list (directory (make-pathname :defaults *background-image-path*
            :name :wild :type :wild :case :common)))
          (*random-state* (make-random-state t)))
      (namestring (nth (random (length file-list)) file-list)))))

(defun print-key-seq (seq) (format nil "^B^9*~{~a~^ ~}^n^1*" (mapcar 'print-key seq)))
(defun display-bindings-for-keymaps (key-seq &rest keymaps)
"Display key-bindings for a given keymap, using a simple and clean format."
  (let* ((screen (current-screen))
         (data (mapcan (lambda (map)
                         (mapcar (lambda (b) (format nil "^B^5*~5a^n ~a" (print-key (binding-key b)) (binding-command b))) (kmap-bindings map)))
                       keymaps))
         (cols (ceiling (1+ (length data))
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "Prefix: ~a~%~{~a~^~%~}"
                        (print-key-seq key-seq)
                        (columnize data cols))))

(defun focus-frame (group f)
"Focus frame but do not show-frame-indicator in certain cases."
  (let ((w (frame-window f))
        (last (tile-group-current-frame group))
        (show-indicator nil))
    (setf (tile-group-current-frame group) f)
    (unless (eq f last)
      (setf (tile-group-last-frame group) last)
      (run-hook-with-args *focus-frame-hook* f last)
      (setf show-indicator t))
    (if w (focus-window w) (no-focus group (frame-window last)))
    (if show-indicator (show-frame-outline group))))

(defun split-frame-in-dir (group dir &optional (ratio 1/2))
  (let ((f (tile-group-current-frame group)))
    (if (split-frame group dir ratio)
        (progn
          (when (frame-window f)
            (update-decoration (frame-window f)))
          (show-frame-indicator group))
        (message "Cannot split smaller than minimum size."))))

(defun run-shell-command-output (command)
"Run a shell command and display results (may hang if used wrong)."
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

#|
(defun expand-file-name (path &optional default-directory)
;"Expand filenames with special focus on home dir."
  (let ((first-char (subseq path 0 1))
    (home-dir *home-dir*)
    (dir (if default-directory
      (if (string= (subseq (reverse default-directory) 0 1) "/")
        default-directory
        (concatenate 'string default-directory "/")))))
  (cond ((string= first-char "~") (concatenate 'string home-dir (subseq path 2)))
        ((string= first-char "/") path)
        (dir (if (strings= (subseq 0 1) "/")
          (concatenate 'string dir path)
          (expand-file-name (concatenate 'string dir path))))
        (t (concatenate 'string home-dir path)))))
|#

(defun eval-command (cmd &optional interactivep)
"Execute a lisp command and display the result, skipping mode-line updates."
  (labels ((parse-and-run-command (input)
             (let* ((arg-line (make-argument-line :string input :start 0))
                    (cmd (argument-pop arg-line)))
               (let ((*interactivep* interactivep))
      (call-interactively cmd arg-line)))))
    (multiple-value-bind (result error-p)
      ;; #(original quote=
      ;; this fancy footwork lets us grab the backtrace from where the error actually happened.)
      (restart-case (handler-bind
          ((error (lambda (c)
                    (invoke-restart 'eval-command-error
                          (format nil "^B^0*{{ ^9*~a ^0*}} ^n~A~a" 
                                cmd c (if *show-command-backtrace* 
                                          (backtrace-string) ""))))))
            (parse-and-run-command cmd))
        (eval-command-error (err-text)
          (values err-text t)))
      (cond ((stringp result)
             (if error-p  (message-no-timeout "~a" result)
                          (message "~a" result)))
            ((eq result :abort)
             (unless *suppress-abort-messages* (message "Abort.")))))))

(defun update-resize-map ()
"Update the (i)resize map, using cleaner key-bindings."
  (let ((m (setf *resize-map* (make-sparse-keymap))))
    (let ((i *resize-increment*))
    (labels ((dk (m k c) (define-key m k (format nil c i))))
      (dk m (kbd "k") "resize 0 -~D")
      (dk m (kbd "(") "resize 0 -~D")
      (dk m (kbd "j") "resize 0 ~D")
      (dk m (kbd ")") "resize 0 ~D")
      (dk m (kbd "h") "resize -~D 0")
      (dk m (kbd "9") "resize -~D 0")
      (dk m (kbd "l") "resize ~D 0")
      (dk m (kbd "0") "resize ~D 0")
      (dk m (kbd "RET") "exit-iresize")
      (dk m (kbd "ESC") "abort-iresize")
    M)))) (update-resize-map)

#|
(defun update-fullscreen (window action)
  (let ((fullscreen-p (window-fullscreen window)))
    (case action
      (0                                ; _NET_WM_STATE_REMOVE
       (when fullscreen-p
         (progn (deactivate-fullscreen window)
         (resize-head 0 0 22 1600 878))))
      (1                                ; _NET_WM_STATE_ADD
       (unless fullscreen-p
         (progn (activate-fullscreen window)
         (resize-head 0 0 0 1600 900))))
      (2                                ; _NET_WM_STATE_TOGGLE
       (if fullscreen-p
           (progn (deactivate-fullscreen window)
           (resize-head 0 0 22 1600 878))
           (progn (activate-fullscreen window)
           (resize-head 0 0 0 1600 900)))))))
|#

