(defcommand go-group (n) ((:number "Go to group: "))
  "Go to selected group, or back to last used one"
  (if (= (slot-value (current-group) 'number) n)
      (gother)
    (run-commands (format nil "gselect ~A" n))))

(defcommand tmux (session) ((:string "Session name: "))
  "Attach tmux session or start a new one.
   Only works with SBCL"
  (run-prog "/usr/bin/tmux"
            :args (list "has-session" "-t" session)
            :wait nil
            #+sbcl :status-hook
            #+sbcl #'(lambda (process)
                       (and (not (sb-ext:process-alive-p process))
                            (if (eq 1 (sb-ext:process-exit-code process))
                                (run-shell-command (format nil "~A -e tmux -2 -f /home/crshd/etc/tmux/tmux.conf new -s ~A" *terminal* session))
                              (run-shell-command (format nil "~A -e tmux attach -t ~A" *terminal* session)))))))

(defcommand send-selection nil nil
  (window-send-string (get-x-selection)))

(defcommand gvim () ()
  "Run-or-Raise GVim"
  (run-or-raise "darkgtk gvim" '(:class "Gvim")))

(defcommand emacs () ()
  "Run-or-Raise Emacs"
  (run-or-raise "emacs" '(:class "Emacs")))

(defcommand firefox () ()
  "Run-or-Raise Firefox"
  (run-or-raise "firefox" '(:class "Firefox")))

(defcommand gimp () ()
  "Run-or-Raise Gimp"
  (run-or-raise "gimp" '(:class "Gimp")))

(defcommand mutt () ()
  "Run-or-Raise Mutt"
  (run-or-raise (format
                 nil
                 "exec ~A -name mutt -e mutt -F ~A/mutt/muttrc"
                 *terminal* (getenv "XDG_CONFIG_HOME"))
                '(:instance "mutt")))

(defcommand showlog (logfile) ((:string "Logfile: "))
  "Show log"
  (run-shell-command (format nil "~A -e tail -f ~A" *terminal* logfile)))

(defcommand logmenu () ()
  "Display menu with log files"
  (labels
      ((pick (options)
             (let ((selection (select-from-menu (current-screen) options "")))
               (cond
                ((null selection)
                 (throw 'error "Abort"))
                ((stringp (second selection))
                 (second selection))
                (t
                 (pick (cdr selection)))))))
    (let ((choice (pick *log-menu*)))
      (run-commands (format nil "showlog ~A" choice)))))

(defparameter *log-menu* '(("STUMP"
                            ("stumpwm.log" "~/var/log/stumpwm.log"))
                           ("XORG"
                            ("Xorg.0.log" "/var/log/Xorg.0.log"))
                           ("EMERGE"
                            ("emerge.log" "/var/log/emerge.log")
                            ("emerge-fetch.log" "/var/log/emerge-fetch.log"))))

(defcommand pimpd (tag value) ((:string "Search for Tag: ")
                               (:string "Search for Value: "))
  "Add to mpd playlist"
  (run-shell-command (format nil "mpc clear; pimpd2 --search-~A ~A | pimpd2 -af" tag value)))

(defcommand pimpdmenu () ()
  "Select Artist/Album/Title search"
  (labels
      ((pick (options)
             (let ((selection (select-from-menu (current-screen) options "")))
               (cond
                ((null selection)
                 (throw 'error "Abort"))
                ((stringp (second selection))
                 (second selection))
                (t
                 (pick (cdr selection)))))))
    (let ((choice (pick *pimpd-menu*)))
      (run-commands (format nil "pimpd ~A" choice)))))

(defparameter *pimpd-menu* '(("Artist" "artist")
                             ("Album" "album")
                             ("Title" "title")))

(defcommand toggle-split () ()
  (let* ((group (current-group))
         (cur-frame (tile-group-current-frame group))
         (frames (group-frames group)))
    (if (eq (length frames) 2)
        (progn (if (or (neighbour :left cur-frame frames)
                       (neighbour :right cur-frame frames))
                   (progn
                     (only)
                     (vsplit))
                 (progn
                   (only)
                   (hsplit))))
      (message "Works only with 2 frames"))))

(defvar *swap-selected-frame* nil
  "Swapping frames yeah!")
(defcommand swap-windows (&optional (frame (tile-group-current-frame (current-group)))) ()
  "Swap to windows. Invoke once to tag, twice to switch selected window with tagged one"
  (if *swap-selected-frame*
      (progn
        (let ((win1 (frame-window *swap-selected-frame*))
              (win2 (frame-window frame)))
          (when win1 (pull-window win1 frame))
          (when win2 (pull-window win2 *swap-selected-frame*)))
        (setf *swap-selected-frame* nil))
    (setf *swap-selected-frame* frame)))
