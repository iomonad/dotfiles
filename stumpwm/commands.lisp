;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../commands.lisp           ;;
;;-----------------------------------------

;; before we go further, rewrite colon command to old behavior.
;; (this should already be fixed in newest version)
(defcommand colon (&optional initial-input) (:rest)
  (let ((cmd (completing-read (current-screen) ": "
          (all-commands) :initial-input (or initial-input ""))))
    (unless cmd
      (throw 'error :abort))
    (when (plusp (length cmd))
      (eval-command cmd t))))

;; redefine run-shell-command for 'zsh', change :shell "", and fix a typo.
(defcommand run-shell-command (cmd &optional collect-output-p)
  ((:shell "execute: "))
  "Run the specified shell command. If @var{collect-output-p} is @code{T}
then run the command synchronously and collect the output."
  (if collect-output-p
    (run-prog-collect-output *shell-program* "-c" cmd)
    (run-prog *shell-program* :args (list "-c" cmd) :wait nil)))
;(setf *shell-program* "/bin/zsh")
(setf *shell-program* (stumpwm::getenv "SHELL"))
(defcommand-alias exec run-shell-command)

;; create a scratchpad group if none exist and toggle between viewing current group and scratchpad group.
;; (idea from Ion3+ window-manager except scratchpad is a group and not a floating frame)
;; (also inspired by another users 'scratchpad' command set, although i found all the functions
;;  and parameters to be wasteful, especially since this creates it per screen anyway)
(defcommand scratchpad () ()
"Create a scratchpad group for current screen, if not found, and toggle between the scatchpad group
and the current group upon reissue of the same command."
  (let* ((sg (find-group (current-screen) *scratchpad-group-name*)) (cg (current-group)))
    (if sg
        (cond ((eq cg sg) (gother)) (t (switch-to-group sg) (message "scratchpad")))
      (progn (gnew *scratchpad-group-name*) (message "scratchpad")))))

;; undo to last state in current-group (set by calling 'remember-group' in various commands/functions),
;; unless no state found. (once i learn more about lisp i'll try removing the use of a second temp file)
(defcommand undo (&optional (group (current-group))) ()
"If an undo state exists for group, revert to last state. Multiple calls toggle between the two states."
  (when group
    (let* ((i (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"
              (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group))))
              :type "lisp" :defaults *undo-data-dir*)))
      (if (probe-file i)
        (progn
          (let* ((o (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"
                    (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group))))
                    :type "bak" :defaults *undo-data-dir*)))
            (dump-group-to-file o)
          (restore-from-file i)
          (rename-file o i)
          (let ((f (tile-group-current-frame group)))
            (progn
              (when (frame-window f)
                (update-decoration (frame-window f)))
              (show-frame-indicator group)))))
        (message "Cannot undo previous state. Nothing found for group ~A" (list (group-name group)))))))

;; dump [current-]group (for current-screen), [current-]screen, desktop or window-placement-rules
;; to a dynamically named file in user defined *data-dir*.
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
        (t (message "don't know how to dump ~a" expr))))

;; restore [current]-group (for current-screen), [current]-screen, desktop or window-placement-rules
;; from a previously created file (more correctly from DUMP-TO-DATADIR) in user defined *data-dir*.
(defcommand restore-from-datadir (expr) (:rest)
"Restore file from data-dir, previously created by 'dump-to-datadir', according to what you specify.
You may restore group (for current-screen), screen (for current-screen), desktop or rules. This will
restore file dynamically by pattern patching, according to what you're restoring, to file name by
looking at what you're currently using.

E.g. if you're in group 2 on screen 0 and you enter 'restore-from-datadir group' it will look for a
file named 'screen_0_group_2.lisp' (created by accompanying 'dump-to-datadir') in your data-dir and
restore it. If no matching file is found it will skip loading of any files and print an error message.

Note: if restoring a group file was successful then an undo state is created so you can technically
undo the loading of that file. There are no undo states when loading 'screen', 'desktop' or 'rules'."
  (cond ((string-equal expr 'group)
          (let* ((i (make-pathname :name (format nil "screen_~{~A~}_group_~{~A~}"
                    (list (char (getenv "DISPLAY") 1)) (list (group-name (current-group))))
                    :type "lisp" :defaults *data-dir*)))
            (if (probe-file i)
                (progn (restore-from-file i) (remember-group) (message "~A restored" expr))
              (message "unable to find valid ~A file in data dir" expr))))
        ((string-equal expr 'screen)
          (let* ((i (make-pathname :name (format nil "screen_~{~A~}" (list (char (getenv "DISPLAY") 1)))
                    :type "lisp" :defaults *data-dir*)))
            (if (probe-file i)
                (progn (restore-from-file i) (message "~A restored" expr))
              (message "unable to find valid ~A file in data dir" expr))))
        ((string-equal expr 'rules)
          (if (probe-file (data-dir-file "tile-rules.lisp"))
              (progn (restore-window-placement-rules (data-dir-file "tile-rules.lisp"))
                     (message "~A restored" expr))
            (message "unable to find valid ~A file in data dir" expr)))
        ((string-equal expr 'desktop)
          (if (probe-file (data-dir-file "desktop.lisp"))
              (progn (restore-from-file (data-dir-file "tile-rules.lisp")) (message "~A restored" expr))
            (message "unable to find valid ~A file in data dir" expr)))
        (t (message "dont know how to restore ~a" expr))))

;; swap neighbors but do not change focus to specified neighbor direction.
(defcommand (exchange-direction-remain tile-group) (dir &optional (win (current-window)))
    ((:direction "Direction: "))
    "If neighbor window exists, swap current window with neighbor in specified direction while
keeping focus on current frame, unlike 'exchange-direction' where focus moves to neighbor."
  (if win
      (let* ((frame-set (group-frames (window-group win)))
             (neighbour (neighbour dir (window-frame win) frame-set)))
        (if (and neighbour (frame-window neighbour))
            (exchange-windows-remain win (frame-window neighbour))
            (message "No window in direction ~A!" dir)))
      (message "No window in current frame!")))

;; move focused window to next/prev group without following it. focus remains on current frame.
(defcommand gmove-next () ()
"Move focused window to next group without switching to it. Unlike behavior in gnext-with-window."
  (move-window-to-next-group (current-group) (sort-groups (current-screen))))
(defcommand gmove-prev () ()
"Move focused window to previous group without switching to it. Unlike behavior in gprev-with-window."
  (move-window-to-next-group (current-group) (reverse (sort-groups (current-screen)))))

;; from simias: rotate windows.
(defcommand rotate-windows () ()
  (let* ((frames (group-frames (current-group)))
            (win (frame-window (car (last frames)))))
          (shift-windows-forward frames win)))

;; rework of original random-bg command, display random wallpaper on root window.
;(defcommand display-random-bg () () "Display a random background image on root window."
;  (run-shell-command
;    (concatenate 'string "display -window root -resize 1600x900! " (select-random-bg-image))))

;; designate master window/frame (should probably use current frame number, but less dynamic?)
(defcommand (master-make tile-group) () () "Designate current window as Master."
  (renumber 0) (repack-window-numbers) (remember-group))
(defcommand (master-focus tile-group) () () "Focus on designated Master window." (select-window-by-number 0))

;; swap current window with master (should be 0 (from master-make)) and desginate it as the new master.
(defcommand (master-swap tile-group) (num &optional (group (current-group))) ((:window-number t))
  "If current window is not Master and Master exists, swap current
window with Master and designate this as the new Master."
  (labels ((match (win)
              (= (window-number win) num)))
  (let ((win (find-if #'match (group-windows group))))
    (when (and win group) (exchange-windows (current-window) win) (master-make)))))

;; [with *shell-program* "/bin/zsh"] look for detached 'tmux [socket] xorg' session and attach, else create new.
;; (also useful for StumpWM crashes, as tmux windows survive crashes and this command brings them back)
(defcommand tmux-attach-else-new () () "Find detached tmux session and attach, else create new session."
  (run-shell-command
  "if [[ -n ${$(tmux -S /tmp/user-keep/${USER}/tmux/xorg list-session|grep -v attached)[1]//:} ]]; then
    urxvt -e tmux -S /tmp/user-keep/${USER}/tmux/xorg attach-session -t ${$(tmux -S /tmp/user-keep/${USER}/tmux/xorg list-session|grep -v attached)[1]//:}
  else
    urxvt -e tmux -S /tmp/user-keep/${USER}/tmux/xorg new-session
  fi"))

;; reassign original commands to *-forget
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

(defcommand restart-soft-forget () () "Soft Restart StumpWM without remembering current state.
The lisp process isn't restarted. Instead, control jumps
to the very beginning of the stumpwm program. This
differs from RESTART, which restarts the unix process.

Since the process isn't restarted, existing customizations remain
after the restart." (throw :top-level :restart))

(defcommand loadrc-forget () () "Reload the @file{~/.stumpwmrc} file without remembering current state."
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
after the restart." (remember-all) (restart-soft-forget))
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

;; redefine resize commands
(defcommand (resize tile-group) (width height) ((:number "+ Width: ")
                                                (:number "+ Height: "))
  "Resize the current frame by @var{width} and @var{height} pixels."
  (let* ((group (current-group))
         (f (tile-group-current-frame group)))
    (if (atom (tile-group-frame-tree group))
        (message "No more frames!")
        (progn
          (clear-frame-outlines group)
          (resize-frame group f width :width)
          (resize-frame group f height :height)
          (draw-frame-outlines group (current-head))
          (curframe))))) (defcommand (iresize tile-group) () ()
  "Remember current state before starting the interactive resize mode. A new keymap
specific to resizing the current frame is loaded. Hit @key{C-g}, @key{RET},
or @key{ESC} to exit." (let ((frame (tile-group-current-frame (current-group))))
    (if (atom (tile-group-frame-head (current-group) (frame-head (current-group) frame)))
        (message "There's only 1 frame!")
        (progn
          (remember-group)
          (when *resize-hides-windows*
            (dolist (f (head-frames (current-group) (current-head)))
              (clear-frame f (current-group))))
          (push-top-map *resize-map*)
          (draw-frame-outlines (current-group) (current-head)))
        ))) (defcommand (exit-iresize tile-group) () ()
  "Exit from the interactive resize mode, quietly." (resize-unhide) (pop-top-map) (redisplay))
(defcommand (quiet-resize tile-group) (width height) ((:number "+ Width: ")
                                                      (:number "+ Height: "))
  "Resize the current frame by @var{width} and @var{height} pixels without highlighting frames."
  (let* ((group (current-group))
         (f (tile-group-current-frame group)))
    (if (atom (tile-group-frame-tree group))
        (message "No more frames!")
        (progn
          (resize-frame group f width :width)
          (resize-frame group f height :height)))))
(defcommand (abort-iresize tile-group) () () "Undo resize changes if aborted."
  (resize-unhide) (undo) (message "Abort resize") (pop-top-map))

;; remove frame and reallocate space while remembering removed frame position, also hiding frame-indicator.
(defcommand (remove-split tile-group)
(&optional (group (current-group)) (frame (tile-group-current-frame group))) ()
"Remove the specified frame in the specified group (defaults to current group, current
frame). Windows in the frame are migrated to the frame taking up its space but not before
remembering their previous positions, also hiding frame highlights."
  (let* ((head (frame-head group frame))
         (current (tile-group-current-frame group))
         (tree (tile-group-frame-head group head))
         (s (closest-sibling (list tree) frame))
         (l (tree-accum-fn s
                           (lambda (&rest siblings)
                             (car siblings))
                           #'identity)))
    ;; <only remove the current frame if it has a sibling>
    (if (atom tree)
        (message "No more frames!")
        (when s
          (remember-group)
          (when (frame-is-head group frame)
            (setf (frame-number l) (frame-number frame)))
          ;; <move the windows from the removed frame to its sibling>
          (migrate-frame-windows group frame l)
          ;; <if the frame has no window, give it the current window of the current frame.>
          (unless (frame-window l)
            (setf (frame-window l)
                  (frame-window frame)))
          ;; <unsplit>
          (setf (tile-group-frame-head group head) (remove-frame tree frame))
          ;; <update the current frame and sync all windows>
          (when (eq frame current)
            (setf (tile-group-current-frame group) l))
          (tree-iterate tree
                        (lambda (leaf)
                          (sync-frame-windows group leaf)))
          (frame-raise-window group l (frame-window l) nil)
          (when (frame-window l)
            (update-decoration (frame-window l)))))))


;; remember states if not already in 'only' mode (e.g., one frame).
(defcommand only () () "Delete all the frames but the current one and grow it
to take up the entire head and remember previous states if entire head
is not already taken up (e.g. already in 'only' mode)."
  (let* ((screen (current-screen))
         (group (screen-current-group screen))
         (win (group-current-window group))
         (head (current-head group))
         (frame (copy-frame head)))
    (if (atom (tile-group-frame-head group head))
      (message "Will not remember state, already using one frame.")
      (progn
        (remember-group)
        (mapc (lambda (w)
                (unless (eq (window-frame w) (tile-group-current-frame group))
                  (hide-window w))
                (setf (window-frame w) frame))
              (head-windows group head))
        (setf (frame-window frame) win
              (tile-group-frame-head group head) frame
              (tile-group-current-frame group) frame)
        (focus-frame group frame)
        (if (frame-window frame)
            (update-decoration (frame-window frame))
            (show-frame-indicator group))
        (sync-frame-windows group (tile-group-current-frame group))))))

;; remember frame positions before splitting (do not edit split-frames function for this)
(defcommand (hsplit tile-group) () () "Remember current state before splitting the
current frame into 2 side-by-side frames." (remember-group) (split-frame-in-dir (current-group) :column))
(defcommand (vsplit tile-group) () ()  "Remember current state before splitting the
current frame into 2 frames, one on top of the other." (remember-group) (split-frame-in-dir (current-group) :row))

;; dump to file, which is silent, but with more informative prompts.
(defcommand dump-group-to-file (file) ((:rest "group to file: "))
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))
(defcommand dump-screen-to-file (file) ((:rest "screen to file: "))
  "Dumps the frames of all groups of the current screen to the named file."
  (dump-to-file (dump-screen (current-screen)) file))
(defcommand dump-desktop-to-file (file) ((:rest "desktop to file: "))
  "Dumps the frames of all groups of all screens to the named file."
  (dump-to-file (dump-desktop) file))

;; predefined echoes for speed, else use 'shell-command-output'.
(defcommand echo-highcpu-user () () "" (message-no-timeout (run-shell-command "ps -U root,privoxy,15,daemon,nobody,unbound --deselect -C tmux,urxvt k -%cpu opid,nice,args:70,etime:10,%cpu,pmem | head -75" t)))
(defcommand echo-highcpu-root () () "" (message-no-timeout (run-shell-command "ps -U h,privoxy,15,daemon,nobody,unbound --deselect -C tmux,urxvt k -%cpu opid,nice,args:70,etime:10,%cpu,pmem | head -75" t)))
(defcommand echo-highcpu-rest () () "" (message-no-timeout (run-shell-command "ps -U root,h --deselect -C tmux,urxvt k -%cpu opid,nice,args:70,etime:10,%cpu,pmem | head -75" t)))
(defcommand echo-mifo-stumpwm () () "" (echo-string (current-screen) (run-shell-command "mifo --announce '(%c/%C)  %D / %b'" t)))
(defcommand echo-mifo-raw () () "" (echo-string (current-screen) (run-shell-command "mifo --announce" t)))
(defcommand echo-mifo-current-list () () "" (echo-string (current-screen) (run-shell-command "mifo --shown |grep -m 1 -A 7 -B 7 $(mifo -a)|sed 's|'$(mifo -a)'|^B^4*&^n|'" t)))
(defcommand echo-mifo-show () () "" (echo-string (current-screen) (run-shell-command "mifo --show" t)))
(defcommand echo-mifo-shown () () "" (echo-string (current-screen) (run-shell-command "mifo --shown" t)))
(defcommand echo-mifo-playlists () () "" (echo-string (current-screen) (run-shell-command "mifo --printp" t)))
(defcommand echo-mifo-add () () "" (echo-string (current-screen) (run-shell-command "mifo --add" t)))
(defcommand echo-mifo-remove () () "" (echo-string (current-screen) (run-shell-command "mifo --remove" t)))
(defcommand echo-mifo-next () () "" (echo-string (current-screen) (run-shell-command "mifo --next ; sleep 1s ; mifo --stumpwm" t)))
(defcommand echo-mifo-prev () () "" (echo-string (current-screen) (run-shell-command "mifo --prev ; sleep 1s ; mifo --stumpwm" t)))
(defcommand echo-mifo-random () () "" (echo-string (current-screen) (run-shell-command "mifo -r ; sleep 1s ; mifo --stumpwm" t)))
(defcommand echo-volume () () "" (echo-string (current-screen) (run-shell-command "pulsevol -a" t)))
(defcommand echo-mail () () "" (echo-string (current-screen) (run-shell-command "print - '^5:'@fea.st: '^B'${#$(find /howl/mail/FastMail/*/new -type f)}" t)))
(defcommand echo-wlan () () "" (echo-string (current-screen) (run-shell-command "ifconfig" t)))
(defcommand echo-free-hdd () () "" (echo-string (current-screen) (run-shell-command "df -hTP -x debugfs" t)))
(defcommand echo-free-mem () () "" (echo-string (current-screen) (run-shell-command "print '^B^3/free^1* used^5* base^n';free -m|awk 'NR==2 {print $4,$3,$2}'" t)))
(defcommand echo-battery () () "" (echo-string (current-screen) (run-shell-command "acpi -tf;repeat 36; do printf '='; done;print;ibam --percentbattery" t)))
(defcommand echo-loadavg () () "" (echo-string (current-screen) (run-shell-command "print ${$(</proc/loadavg)[1,3]}" t)))
(defcommand echo-colors-brief () () "Output a brief list of currently defined colors." (echo-string (current-screen) (eval "
BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n")))
(defcommand echo-colors-full () () "Output a full list of currently defined colors." (echo-string (current-screen) (eval "
BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
B->0 ^B^00black^n ^B^10red^n ^B^20green^n ^B^30yellow^n ^B^40blue^n ^B^50magenta^n ^B^60cyan^n ^B^70white^n ^B^80user^n ^B^90user^n
N->0 ^00black^n ^10red^n ^20green^n ^30yellow^n ^40blue^n ^50magenta^n ^60cyan^n ^70white^n ^80user^n ^90user^n
B->1 ^B^01black^n ^B^11red^n ^B^21green^n ^B^31yellow^n ^B^41blue^n ^B^51magenta^n ^B^61cyan^n ^B^71white^n ^B^81user^n ^B^91user^n
N->1 ^01black^n ^11red^n ^21green^n ^31yellow^n ^41blue^n ^51magenta^n ^61cyan^n ^71white^n ^81user^n ^91user^n
B->2 ^B^02black^n ^B^12red^n ^B^22green^n ^B^32yellow^n ^B^42blue^n ^B^52magenta^n ^B^62cyan^n ^B^72white^n ^B^82user^n ^B^92user^n
N->2 ^02black^n ^12red^n ^22green^n ^32yellow^n ^42blue^n ^52magenta^n ^62cyan^n ^72white^n ^82user^n ^92user^n
B->3 ^B^03black^n ^B^13red^n ^B^23green^n ^B^33yellow^n ^B^43blue^n ^B^53magenta^n ^B^63cyan^n ^B^73white^n ^B^83user^n ^B^93user^n
N->3 ^03black^n ^13red^n ^23green^n ^33yellow^n ^43blue^n ^53magenta^n ^63cyan^n ^73white^n ^83user^n ^93user^n
B->4 ^B^04black^n ^B^14red^n ^B^24green^n ^B^34yellow^n ^B^44blue^n ^B^54magenta^n ^B^64cyan^n ^B^74white^n ^B^84user^n ^B^94user^n
N->4 ^04black^n ^14red^n ^24green^n ^34yellow^n ^44blue^n ^54magenta^n ^64cyan^n ^74white^n ^84user^n ^94user^n
B->5 ^B^05black^n ^B^15red^n ^B^25green^n ^B^35yellow^n ^B^45blue^n ^B^55magenta^n ^B^65cyan^n ^B^75white^n ^B^85user^n ^B^95user^n
N->5 ^05black^n ^15red^n ^25green^n ^35yellow^n ^45blue^n ^55magenta^n ^65cyan^n ^75white^n ^85user^n ^95user^n
B->6 ^B^06black^n ^B^16red^n ^B^26green^n ^B^36yellow^n ^B^46blue^n ^B^56magenta^n ^B^66cyan^n ^B^76white^n ^B^86user^n ^B^96user^n
N->6 ^06black^n ^16red^n ^26green^n ^36yellow^n ^46blue^n ^56magenta^n ^66cyan^n ^76white^n ^86user^n ^96user^n
B->7 ^B^07black^n ^B^17red^n ^B^27green^n ^B^37yellow^n ^B^47blue^n ^B^57magenta^n ^B^67cyan^n ^B^77white^n ^B^87user^n ^B^97user^n
N->7 ^07black^n ^17red^n ^27green^n ^37yellow^n ^47blue^n ^57magenta^n ^67cyan^n ^77white^n ^87user^n ^97user^n
B->8 ^B^08black^n ^B^18red^n ^B^28green^n ^B^38yellow^n ^B^48blue^n ^B^58magenta^n ^B^68cyan^n ^B^78white^n ^B^88user^n ^B^98user^n
N->8 ^08black^n ^18red^n ^28green^n ^38yellow^n ^48blue^n ^58magenta^n ^68cyan^n ^78white^n ^88user^n ^98user^n
B->9 ^B^09black^n ^B^19red^n ^B^29green^n ^B^39yellow^n ^B^49blue^n ^B^59magenta^n ^B^69cyan^n ^B^79white^n ^B^89user^n ^B^99user^n
N->9 ^09black^n ^19red^n ^29green^n ^39yellow^n ^49blue^n ^59magenta^n ^69cyan^n ^79white^n ^89user^n ^99user^n")))

;; sent output of command to echo-string (may hang if used wrong).
(defcommand shell-command-output (command) ((:string "execute/output: "))
  "Take output of command and display it. This may hang if used wrong."
  (check-type command string) (run-shell-command-output command))
(defcommand pout (&optional (initial "")) (:rest)
  "Prompt with the given argument as command, await any additional arguments
and then run as shell command, displaying a message with any of the
command's output. This may hang if used wrong."
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd (shell-command-output cmd))))

;; manpage reader. needs filename completion, etc.. very simple right now
(defcommand manpage (command) ((:rest "manpage: ")) ""
  (run-shell-command (format nil "urxvt -e man ~a" command)))

;; prompt for X selection to transfer, or prompt for X selection to echo
(defcommand prompt-xclip (filename) ((:rest "xclip -selection ")) ""
  (run-shell-command (format nil "xclip -selection ~a" filename)))
;; prompt for X selection to display contents of.
(defcommand echo-xclip (filename) ((:rest "echo.selection: ")) ""
  (echo-string (current-screen) (run-shell-command (format nil "xclip -selection ~a -o" filename) t)))

;; i don't like 'Colon' showing editable command in prompt
;; perhaps i'll figure out a global macro/function for this..
(defcommand prompt-mifo-command (filename) ((:rest "mifo.command: ")) ""
  (run-shell-command (format nil "mifo --command ~a" filename)))
(defcommand prompt-mifo-next (filename) ((:rest "mifo.next: ")) ""
  (echo-string (current-screen) (run-shell-command (format nil "mifo --next ~a && sleep 2" filename) t))
  (echo-mifo-stumpwm))
(defcommand prompt-mifo-prev (filename) ((:rest "mifo.previous: ")) ""
  (echo-string (current-screen) (run-shell-command (format nil "mifo --prev ~a && sleep 2" filename) t))
  (echo-mifo-stumpwm))
(defcommand prompt-mifo-save (filename) ((:rest "mifo.save-as: ")) ""
  (echo-string (current-screen) (run-shell-command (format nil "mifo --save ~a" filename) t)))
(defcommand prompt-mifo-find (match) ((:rest "mifo.find: ")) ""
  (echo-string (current-screen) (run-shell-command (format nil "mifo --find ~a" match) t)))
(defcommand prompt-mifo-findn (match) ((:rest "mifo.findn: ")) ""
  (echo-string (current-screen) (run-shell-command (format nil "mifo --findn ~a" match) t)))
(defcommand prompt-mifo-load (filename) ((:rest "mifo.load: "))
  (run-shell-command (format nil "mifo --load ~a" filename))) ""
(defcommand prompt-mifo-append (filename) ((:rest "mifo.append: "))
  (run-shell-command (format nil "mifo --append ~a" filename))) ""
(defcommand prompt-mifo-playlist (filename) ((:rest "mifo.playlist: "))
  (run-shell-command (format nil "mifo --playlist ~a" filename))) ""
(defcommand prompt-mifo-begin (filename) ((:rest "mifo.begin: ")) ""
  (run-shell-command (format nil "mifo --begin ~a" filename)))
(defcommand prompt-mifo-seek (filename) ((:rest "mifo.seek: ")) ""
  (run-shell-command (format nil "mifo --seek ~a" filename)))

;; evaluate string, with prettier color.
(defcommand eval-line (cmd) ((:rest "eval: "))
  "Evaluate the s-expression and display the result(s)."
  (handler-case
    (message "^B^10~{~a~^~%~}"
      (mapcar 'prin1-to-string
        (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^5*~A" c))))

;; run or raise.
(defcommand ror_dwb () () "" (setf *run-or-raise-all-groups* t) (run-or-raise "dwb" '(:class "Dwb")))
(defcommand ror_mutt () () "" (setf *run-or-raise-all-groups* t)
  (run-or-raise "urxvt -title '[urxvt] mutt' -e mutt -F ${XDG_CONFIG_DIR:-${HOME}}/mutt/muttrc" '(:title "\\[urxvt\\] mutt")))

;; EOF
