
(defcommand quit-forget () () "Quit StumpWM without remembering current state."
  (with-open-file (stream *debug-file* :direction :io :if-exists :supersede))
  (cond ((find-group (current-screen) *scratchpad-group-name*)
         (if (eq (current-group) (find-group (current-screen) *scratchpad-group-name*))
             (gkill)
           (progn
             (gnext) (kill-group
                      (find-group (current-screen) *scratchpad-group-name*)
                      (current-group))))))
  (throw :top-level :quit))

(defcommand restart-soft-forget () () 
  "Soft Restart StumpWM without remembering current state.
The lisp process isn't restarted. Instead, control jumps
to the very beginning of the stumpwm program. This
differs from RESTART, which restarts the unix process.

Since the process isn't restarted, existing customizations remain
after the restart." 
  (throw :top-level :restart))

(defcommand loadrc-forget () () "Reload the @file{~/.stumpwmrc} file without remember current state."
  (handler-case
   (progn
     (with-restarts-menu (load-rc-file nil)))
   (error (c)
          (message "^B^1*Error loading rc file:^n ~A" c))
   (:no-error (&rest args)
              (declare (ignore args))
              (message "rc file loaded successfully."))))

(defcommand loadrc () () "Reload the @file{~/.stumpwmrc} file while remembering current state."
  (remember-all) (loadrc-forget))

(defcommand restart-soft () ()
  "Soft Restart StumpWM while remembering current state.
The lisp process isn't restarted. Instead, control jumps
to the very beginning of the stumpwm program. This
differs from RESTART, which restarts the unix process.

Since the process isn't restarted, existing customizations remain
after the restart." 
  (remember-all) (restart-soft-forget))

(defcommand-alias restart restart-soft)

(defcommand quit () ()
  "Quit StumpWM while remembering current state."
  (cond ((find-group (current-screen) *scratchpad-group-name*)
         (if (eq (current-group) (find-group (current-screen) *scratchpad-group-name*))
             (gkill)
           (progn
             (gnext) (kill-group
                      (find-group (current-screen) *scratchpad-group-name*)
                      (current-group))))))
  (remember-all) (quit-forget))


(defun remember-all () ()
"Similiar to remember-group except all information is dumped, useful
for next startup or recalling all undo actions."
  (dump-to-datadir "rules") (dump-to-datadir "desktop"))

(defcommand dump-to-datadir (expr) (:rest)
  "Dump group (from current-screen), screen (current-screen), desktop or rules to file in data-dir.
Just specify what you want to dump and this will dynamically create and name file accordingly."
  (cond ((string-equal expr 'group)
         (let* ((o (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"
                                                (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group))))
                                  :type "lisp" :defaults *data-dir*)))
           (dump-group-to-file o) (message "~A dumped" expr)))
        ((string-equal expr 'screen)
         (let* ((o (make-pathname :name (format nil "screen_~{~A~}" (list (char (getenv "DISPLAY") 1)))
                                  :type "lisp" :defaults *data-dir*)))
           (dump-screen-to-file o) (message "~A dumped" expr)))
        ((string-equal expr 'rules)
         (let* ((o (make-pathname :name "tile-rules" :type "lisp" :defaults *data-dir*)))
           (dump-window-placement-rules o) (message "~A dumped" expr)))
        ((string-equal expr 'desktop)
         (let* ((o (make-pathname :name "desktop" :type "lisp" :defaults *data-dir*)))
           (dump-desktop-to-file o) (message "~A dumped" expr)))
        (t (message "dont know how to dump ~a" expr))))

(defcommand dump-group-to-file (file) ((:rest "group to file: "))
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))
(defcommand dump-screen-to-file (file) ((:rest "screen to file: "))
  "Dumps the frames of all groups of the current screen to the named file."
  (dump-to-file (dump-screen (current-screen)) file))
(defcommand dump-desktop-to-file (file) ((:rest "desktop to file: "))
  "Dumps the frames of all groups of all screens to the named file."
  (dump-to-file (dump-desktop) file))



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

(defun fmt-group-status (group)
  (let ((screen (group-screen group)))
    (cond ((eq group (screen-current-group screen))
           #\*)
          ((and (typep (second (screen-groups screen)) 'group)
                (eq group (second (screen-groups screen))))
           #\+)
          (t #\-))))
