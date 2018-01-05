;;--------------------------------------------------------------------------;;
;; ${XDG_CONFIG_DIR:-/howl/conf}/.stumpwmrc                                 ;;
;;--------------------------------------------------------------------------;;
;; author: milomouse <vincent[at]fea.st>                                    ;;
;; update: 2011-06-21 23:01:09                                              ;;
;;--------------------------------------------------------------------------;;
;; versions used atoc:                                                      ;;
;; |  sbcl              -> 1.0.49-1                                         ;;
;; |  clx               -> 0.7.4-1                                          ;;
;; |  cl-ppcre          -> 2.0.3-1                                          ;;
;; |  stumpwm-git       -> 20110617-1                                       ;;
;;-TODO/CHANGELOG:----------------------------------------------------------;;
;; >>>-: create a 'dedicate' and 'catchall' window-rule (remember * *)      ;;
;; >>--: create a 'dedicate' and 'catchall' hook for changing focus color   ;;
;; >>--: have mifo(mplayer-daemon) prompts use filename completion          ;;
;; >---: better resize; if neighbour {above} then -ARG else +ARG, etc.      ;;
;; >>>-: show frame-indicator for 'resize' only if no window in frame       ;;
;; >>>>? command for dedicating current win/frame as the Master win/frame   ;;
;; >---: better command for Master; remember Master thru re{loadrc,start}   ;;
;; >>>>! command for swapping current window with the Master win/frame      ;;
;; >>>>! exchange two windows but keep focus in current frame               ;;
;; >>>>! dynamically dump group|screen|desktop|rules to *data-dir* by type  ;;
;; >>>>! dynamically load group|screen|desktop|rules from *data-dir* by type;;
;; >>>>! have my 'undo' use group and check current group for undos first   ;;
;; >>>>! create a togglable, on-the-fly scratchpad group                    ;;
;;--------------------------------------------------------------------------;;
;; files: *data-dir*/../{commands,functions,hooks,key-maps,macros}.lisp     ;;
;;--------------------------------------------------------------------------;;

;; bulk version (config usually broken up in multiple. see loop below.)

(in-package :stumpwm)

;; set a home-dir. not relative *default-pathname-defaults*.
;; set a data-dir for storing debug-file, group and placement dumps, etc.
;; set undo directory to store each group (and desktop) undo states.
;; set scratchpad group name for when it's created on the fly (.NAME to hide)
(setf *home-dir* (make-pathname :directory "/howl")
      *data-dir* (merge-pathnames (make-pathname :directory
                 '(:relative "conf" "stumpwm" "storage")) *home-dir*)
      *undo-data-dir* (make-pathname :directory "/dev/shm/.1009")
      *scratchpad-group-name* ".scratchpad"
      *debug-level* 1)

;; setup a quick function for redirecting debug information directly to file.
;; (didn't want to use (redirect-all-output) as that's not what i want..)
;; (prefer internal handling as opposed to redirecting via exec $ >>! file)
(defvar *debug-restream* nil)
(defun redirect-debug (file) "Redirect *debug-stream* directly to a file."
  (when (typep *debug-restream* 'file-stream)
    (close *debug-restream*))
  (setf *debug-restream* (open file :direction :output :if-exists :append
                         :if-does-not-exist :create)
        *debug-stream* *debug-restream*))

;; setup debug-file variable for referencing (e.g. quitting) purposes.
(defvar *debug-file* (data-dir-file "log" "lisp"))
(redirect-debug *debug-file*)

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
(setf *shell-program* "/bin/zsh")
;(setf *shell-program* (stumpwm::getenv "SHELL"))
(defcommand-alias exec run-shell-command)

;; define a background-image-path for random image setting function.
;; (will soon change this to accept optional sub-dir for situations where
;; user wants to use 'work' or 'family' wallpapers instead)
(defvar *background-image-path*
  (merge-pathnames
    (make-pathname :directory '(:relative "foto" "wall")) *home-dir*))

;; gravities.
(setf *mouse-focus-policy* :click
      *window-border-style* :thin
      *message-window-gravity* :top-right
      *input-window-gravity* :top-right)
(set-normal-gravity :top) ; top for terminals
(set-maxsize-gravity :center) ; center for floating X apps
(set-transient-gravity :center) ; center for save-as/open popups

;; borders.
(setf *resize-hides-windows* T
      *normal-border-width* 2
      *maxsize-border-width* 2
      *transient-border-width* 2
      *float-window-border* 1
      *float-window-title-height* 1)
(set-msg-border-width 1)

;; fonts/colors.
(set-font "-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r")
(set-fg-color        "grey64")
(set-bg-color        "grey14")
(set-focus-color     "grey60")
;(set-focus-color     "mediumpurple2")
(set-unfocus-color   "grey16")
(set-border-color    "grey44")
(set-win-bg-color    "grey6")
(setf *colors* (list "grey9"          ; 0 black
                     "palevioletred1" ; 1 red
                     "lightblue3"     ; 2 green
                     "bisque3"        ; 3 yellow
                     "steelblue3"     ; 4 blue
                     "slateblue1"     ; 5 magenta
                     "aquamarine4"    ; 6 cyan
                     "honeydew4"      ; 7 white
                     "thistle4"       ; 8 user
                     "lightskyblue4")); 9 user
(update-color-map (current-screen))

;; text formatting (no mode-line). shorten time-day-names, etc.
(setf *startup-message*
       "^B^1*together we ate the king^n:^B^5*and laughed ourselves to death^n"
      *time-day-names* #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
      *time-format-string-default* "^B^2*%T^9* %Y-%m-%d^**/^8*%A^n"
      *group-format*    "^B^0*%t^7*%s^07|^n"
      *window-format*   "^B^87%s^9*%m^0*%16t^n"
      *screen-mode-line-format* nil
      *timeout-wait* 6)

;; windows/frames. use more intuitive frame-numbers (with 'fselect').
(setf *default-window-name* "null"
      *new-frame-action* :empty
      *min-frame-width* 45
      *min-frame-height* 45
      *resize-increment* 2
      *frame-number-map* "yhjukilop")

;; mode-line and input.
(setf *mode-line-background-color* "grey5"
      *mode-line-border-color* "grey10"
      *mode-line-foreground-color* "azure4"
      *mode-line-border-width* 1
      *mode-line-pad-x* 1
      *mode-line-pad-y* 0
      *mode-line-timeout* 300
      *mode-line-position* :top
      *input-history-ignore-duplicates* 0)

;; bulk version (skip loading external files; already appended below.)
;; load external settings files. these are the bulk of setup/optimizations.
;(loop for file in '("functions" "macros" "commands" "hooks" "key-maps")
;  do (load (merge-pathnames (make-pathname :name file :type "lisp"
;           :directory '(:relative "conf" "stumpwm")) *home-dir*)))

;; create given groups while keeping focus on current.
(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
  `(run-commands ,@ns)))

;; restore data from previous exit (state StumpWM was last using),
(clear-window-placement-rules)
(setf (group-name (first (screen-groups (current-screen)))) "1")
(make-groups-bg "2" "3" "4" "5" "6")
(if (probe-file (data-dir-file "desktop.lisp"))
    (restore-from-file (data-dir-file "desktop.lisp")))
(restore-window-placement-rules (data-dir-file "tile-rules.lisp"))
(cond ((string-equal (group-name (current-group)) *scratchpad-group-name*) (gother)))

;; display a random background image on root window.
;(display-random-bg)

;; EOF
;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../functions.lisp          ;;
;;-----------------------------------------

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

(defun split-frame-in-dir (group dir)
  (let ((f (tile-group-current-frame group)))
    (if (split-frame group dir)
        (progn
          (update-decoration (frame-window f)))
        (message "Canot split smaller than minimum size."))))

(defun run-shell-command-output (command)
"Run a shell command and display results (may hang if used wrong)."
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

;(defun expand-file-name (path &optional default-directory)
;;"Expand filenames with special focus on home dir."
;  (let ((first-char (subseq path 0 1))
;    (home-dir *home-dir*)
;    (dir (if default-directory
;      (if (string= (subseq (reverse default-directory) 0 1) "/")
;        default-directory
;        (concatenate 'string default-directory "/")))))
;  (cond ((string= first-char "~") (concatenate 'string home-dir (subseq path 2)))
;        ((string= first-char "/") path)
;        (dir (if (strings= (subseq 0 1) "/")
;          (concatenate 'string dir path)
;          (expand-file-name (concatenate 'string dir path))))
;        (t (concatenate 'string home-dir path)))))

(defun eval-command (cmd &optional interactivep)
"Execute a lisp command and display the result, skipping mode-line updates."
  (labels ((parse-and-run-command (input)
             (let* ((arg-line (make-argument-line :string input :start 0))
                    (cmd (argument-pop arg-line)))
               (let ((*interactivep* interactivep))
     (call-interactively cmd arg-line)))))
    (multiple-value-bind (result error-p)
      ;; <original quote=
      ;; this fancy footwork lets us grab the backtrace from where the error actually happened.>
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

;; incomplete, was just testing alsa out..
;(defcommand amixer-control (channel arg)
;  (let ((variance (run-shell-command (concatenate 'string
;      "print ${$(amixer sget " channel ")[-2,-1]//(\[|\]|.*dB|-)}"))))
;    (cond ((and (eq channel "PCM") (not (eq arg "toggle")))
;          (message (first (concatenate 'string variance))))
;          (t (message (second (concatenate 'string variance))))
;          )))

;; EOF
;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../macros.lisp             ;;
;;-----------------------------------------

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
;; frame raise lock (lock AND raise == jumpto)

;; internet related workspace:
(define-frame-preference "3"
  (0    nil   t     :instance "luakit")
  (0    nil   t     :instance "Navigator"))

;; largely undefined, temporal workspace:
(define-frame-preference "6"
  (0    t     t     :class "Ossxmix")
  (0    t     t     :class "Gliv"))

;; EOF
;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../commands.lisp           ;;
;;-----------------------------------------

;; create a scratchpad group if none exist and toggle between viewing current group and scratchpad group.
;; (idea from Ion3+ window-manager except scratchpad is a group and not a floating frame)
;; (also inspired by another users 'scratchpad' command set, although i found all the functions
;;  and parameters to be wasteful, especially since it's created per screen anyway(?:[untested aspect]))
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
          (rename-file o i)))
        (message "Cannot undo previous state. Nothing found for group ~A" (list (group-name group)))))))

;; dump [current]-group (for current-screen), [current]-screen, desktop or window-placement-rules
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
        (t (message "dont know how to dump ~a" expr))))

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
(defcommand display-random-bg () () "Display a random background image on root window."
  (run-shell-command
    (concatenate 'string "display -window root -resize 1600x900! " (select-random-bg-image))))

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
;; (useful for StumpWM crashes, as tmux windows survive crashes and this command brings them back)
(defcommand tmux-attach-else-new () () "Find detached tmux session and attach, else create new session."
  (run-shell-command
  "if [[ -n ${$(tmux -S /tmp/.${UID}/tmux/xorg list-session|grep -v attached)[1]//:} ]]; then
    urxvt -e tmux -S /tmp/.${UID}/tmux/xorg attach-session -t ${$(tmux -S /tmp/.${UID}/tmux/xorg list-session|grep -v attached)[1]//:}
  else
    urxvt -e tmux -S /tmp/.${UID}/tmux/xorg new-session
  fi"))

;; [with *shell-program* "/bin/zsh"] look for detached 'tmux [socket] rtorrent' session and attach, else nothing.
(defcommand tmux-attach-rtorrent () () "Find detached rtorrent session and attach, else not running so do nothing."
  (run-shell-command
  "if [[ -n ${$(tmux -S /tmp/.${UID}/tmux/rtorrent list-session|grep -v attached)[1]//:} ]]; then
    urxvt -e tmux -S /tmp/.${UID}/tmux/rtorrent attach-session -t ${$(tmux -S /tmp/.${UID}/tmux/rtorrent list-session|grep -v attached)[1]//:}
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
(defcommand echo-mifo-stumpwm () () "" (echo-string (current-screen) (run-shell-command "mifo --stumpwm" t)))
(defcommand echo-mifo-raw () () "" (echo-string (current-screen) (run-shell-command "mifo --raw" t)))
(defcommand echo-mifo-current-list () () "" (echo-string (current-screen) (run-shell-command "mifo --show current|grep -A 7 -B 7 $(mifo --raw)|sed 's|'$(mifo --raw)'|^B^1*&^n|'" t)))
(defcommand echo-mifo-playlists () () "" (echo-string (current-screen) (run-shell-command "mifo --show" t)))
(defcommand echo-mifo-fav-add () () "" (echo-string (current-screen) (run-shell-command "mifo --fav-add" t)))
(defcommand echo-mifo-fav-del () () "" (echo-string (current-screen) (run-shell-command "mifo --fav-delete" t)))
(defcommand echo-mifo-next () () "" (echo-string (current-screen) (run-shell-command "mifo --next ; sleep 1 ; mifo --stumpwm" t)))
(defcommand echo-mifo-prev () () "" (echo-string (current-screen) (run-shell-command "mifo --prev ; sleep 1 ; mifo --stumpwm" t)))
(defcommand echo-mifo-random () () "" (echo-string (current-screen) (run-shell-command "mifo -r ; sleep 1 ; mifo --stumpwm" t)))
(defcommand echo-oss-vol () () "" (echo-string (current-screen) (run-shell-command "ossvol -a" t)))
(defcommand echo-oss-volup () () "" (run-shell-command "ossvol -i 1") (echo-oss-vol))
(defcommand echo-oss-voldown () () "" (run-shell-command "ossvol -d 1") (echo-oss-vol))
(defcommand echo-oss-volmute () () "" (run-shell-command "ossvol -m"))
(defcommand echo-oss-speakers () () "" (echo-string (current-screen) (run-shell-command "ossvol --speakers --quiet" t)) (echo-oss-vol))
(defcommand echo-oss-headphones () () "" (run-shell-command "ossvol --headphones --quiet") (echo-oss-vol))
(defcommand echo-mail () () "" (echo-string (current-screen) (run-shell-command "print - @fea.st: ${#$(find /howl/mail/FastMail/*/new -type f)}" t)))
(defcommand echo-wlan () () "" (echo-string (current-screen) (run-shell-command "iwconfig wlan0" t)))
(defcommand echo-free-hdd () () "" (echo-string (current-screen) (run-shell-command "di -x debugfs,tmpfs -d h -Af SMTufI" t)))
(defcommand echo-free-mem () () "" (echo-string (current-screen) (run-shell-command "print '^B^6/free^1* used^5* base^n';free -m|awk 'NR==2 {print $4,$3,$2}'" t)))
(defcommand echo-battery () () "" (echo-string (current-screen) (run-shell-command "acpi -tf;repeat 36; do printf '='; done;print;ibam --percentbattery" t)))
(defcommand echo-loadavg () () "" (echo-string (current-screen) (run-shell-command "print ${$(</proc/loadavg)[1,3]}" t)))
(defcommand echo-colors-brief () () "Output a brief list of currently defined colors." (echo-string (current-screen) (eval "
BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n")))

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
(defcommand prompt-mifo-load (filename) ((:rest "mifo.load: "))
  (run-shell-command (format nil "mifo --load ~a" filename))) ""
(defcommand prompt-mifo-append (filename) ((:rest "mifo.append: "))
  (run-shell-command (format nil "mifo --append ~a" filename))) ""
(defcommand prompt-mifo-playlist (filename) ((:rest "mifo.playlist: "))
  (run-shell-command (format nil "mifo --playlist ~a" filename))) ""
(defcommand prompt-mifo-reload (filename) ((:rest "mifo.reload: ")) ""
  (run-shell-command (format nil "mifo --reload ~a" filename)))

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
;;(defcommand ror_firefox () () "" (setf *run-or-raise-all-groups* t) (run-or-raise "firefox" '(:instance "Navigator")))
;;(defcommand ror_jumanji () () "" (setf *run-or-raise-all-groups* t) (run-or-raise "jumanji" '(:class "Jumanji")))
(defcommand ror_luakit () () "" (setf *run-or-raise-all-groups* t) (run-or-raise "luakit" '(:class "luakit")))
(defcommand ror_mutt () () "" (setf *run-or-raise-all-groups* t)
  (run-or-raise "urxvt -title '[urxvt] mutt' -e mutt -F ${XDG_CONFIG_DIR:-${HOME}}/mutt/muttrc" '(:title "\\[urxvt\\] mutt")))

;; EOF
;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../hooks.lisp              ;;
;;-----------------------------------------

;; show local windows in frame when focusing on it. unfortunately the echo
;; command is cropped when focused frame overlaps part of it's output.
;(defun local-list (from-frame to-frame)
;  (run-commands "echo-frame-windows"))
;add-hook *focus-frame-hook* 'local-list)

;; display the keysequence in progress (found this).
;(defun key-press-hook (key key-seq cmd)
;  (declare (ignore key))
;  (unless (eq *top-map* *resize-map*)
;    (let ((*message-window-gravity* :top-right))
;      (message "Key sequence: ~A" (print-key-seq (reverse key-seq))))
;    (when (stringp cmd)
;      (sleep 0.1))))
;(replace-hook *key-press-hook* 'key-press-hook)

;; EOF
;;-----------------------------------------
;; author: milomouse <vincent[at]fea.st> ;;
;; *data-dir*/../key-maps.lisp           ;;
;;-----------------------------------------

;; export custom maps.
(export '(*echo-map* *xsel-map* *xclip-clipboard-map* *xclip-primary-map*
          *frequent-map* *win-frame-map* *mplayer-map1* *mplayer-map2*))

;; set a few undefined keysyms, unavailable in */stumpwm/keysyms.lisp
(define-keysym #x1008ff02 "XF86MonBrightnessUp")
(define-keysym #x1008ff03 "XF86MonBrightnessDown")

;; set "Super+Shift+\" as prefix for root-map bindings (this will not be used)
(set-prefix-key (kbd "s-|"))

;; some useful window/frame commands.
(defvar *win-frame-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "r")   "remember")
    (dk m (kbd "f")   "forget")
    (dk m (kbd "p")   "place-existing-windows")
    (dk m (kbd "n")   "repack-window-numbers")
    (dk m (kbd "ESC") "abort")
   M)))

;; transfer contents of clipboard into other buffers, or manually type cmd.
(defvar *xclip-clipboard-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "b") "exec xclip -selection clipboard -o | xclip -selection buffer-cut -i")
    (dk m (kbd "p") "exec xclip -selection clipboard -o | xclip -selection primary -i")
    (dk m (kbd "s") "exec xclip -selection clipboard -o | xclip -selection secondary -i")
    (dk m (kbd ";") "prompt-xclip")
    (dk m (kbd ":") "echo-xclip")
    (dk m (kbd "ESC") "abort")
   M)))

(defvar *xclip-primary-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "b") "exec xclip -selection primary -o | xclip -selection buffer-cut -i")
    (dk m (kbd "c") "exec xclip -selection primary -o | xclip -selection clipboard -i")
    (dk m (kbd "s") "exec xclip -selection primary -o | xclip -selection secondary -i")
    (dk m (kbd ";") "prompt-xclip")
    (dk m (kbd ":") "echo-xclip")
    (dk m (kbd "ESC") "abort")
   M)))

;; interact with the xselection and meta commands.
(defvar *xsel-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "c")   "copy-last-message")
    (dk m (kbd "g")   "getsel")
    (dk m (kbd "m")   "meta")
    (dk m (kbd "p")   "putsel")
    (dk m (kbd "s")   "window-send-string")
    (dk m (kbd "ESC") "abort")
  M)))

;; frequently used echoes for quick information grabbing.
(defvar *echo-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "b")   "echo-battery")
    (dk m (kbd "c")   "echo-colors-brief")
    (dk m (kbd "d")   "echo-date")
    (dk m (kbd "f")   "echo-free-mem")
    (dk m (kbd "h")   "echo-free-hdd")
    (dk m (kbd "l")   "echo-loadavg")
    (dk m (kbd "m")   "echo-mifo-stumpwm")
    (dk m (kbd "M")   "echo-mifo-raw")
    (dk m (kbd "C-m") "echo-mifo-current-list")
    (dk m (kbd "n")   "echo-wlan")
    (dk m (kbd "p")   "echo-highcpu-user")
    (dk m (kbd "P")   "echo-highcpu-root")
    (dk m (kbd "C-p") "echo-highcpu-rest")
    (dk m (kbd "u")   "echo-mail")
    (dk m (kbd "v")   "echo-oss-vol")
    (dk m (kbd "w")   "pout exec sdcv -nu WordNet ")
    (dk m (kbd "W")   "pout exec sdcv -nu \"English Thesaurus\" ")
    (dk m (kbd "ESC") "abort")
   M)))

;; frequently used commands.
(defvar *frequent-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "2") "echo-oss-speakers")
    (dk m (kbd "3") "echo-oss-headphones")
    (dk m (kbd "b") "display-random-bg")
    (dk m (kbd "B") "exec display -window root -resize 1600x900! /howl/foto/wall/wallpaper-31278.png")
    (dk m (kbd "C-b") "exec display -window root -resize 1600x900! /howl/foto/wall/1366x768_dizorb_landscape_study_hd_wallpaper.png")
    (dk m (kbd "d") "exec xmodmap /howl/conf/keymaps/dvausler-mod.xmodmap")
    (dk m (kbd "h") "exec urxvt -e htop")
    (dk m (kbd "l") "ror_luakit")
    (dk m (kbd "m") "ror_mutt")
    (dk m (kbd "q") "exec xmodmap /howl/conf/keymaps/qwerty.xmodmap")
    (dk m (kbd "s") "exec urxvt -e nsudoku 12")
    (dk m (kbd "x") "exec xskat -opt /howl/conf/xorg/xskat.opt -list /howl/conf/xorg/xskat.lst")
    (dk m (kbd "ESC") "abort")
   M)))

;; mplayer daemon (mifo) frequently used commands.
(defvar *mplayer-map1*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "0")     "exec mifo --reload 0")
    (dk m (kbd "1")     "exec mifo --reload 1")
    (dk m (kbd "a")     "prompt-mifo-load")
    (dk m (kbd "A")     "prompt-mifo-append")
    (dk m (kbd "d")     "exec sudo /etc/rc.d/mifo start")
    ;(dk m (kbd "d")     "exec mifo --daemon")
    (dk m (kbd "f")     "exec mifo --fullscreen")
    (dk m (kbd "h")     "echo-mifo-prev")
    (dk m (kbd "H")     "prompt-mifo-prev")
    (dk m (kbd "j")     "prompt-mifo-next +")
    (dk m (kbd "k")     "prompt-mifo-prev dir")
    (dk m (kbd "l")     "echo-mifo-next")
    (dk m (kbd "L")     "prompt-mifo-next")
    (dk m (kbd "p")     "prompt-mifo-playlist")
    (dk m (kbd "P")     "echo-mifo-playlists")
    (dk m (kbd "q")     "exec sudo /etc/rc.d/mifo stop")
    ;(dk m (kbd "q")     "exec mifo --quit")
    (dk m (kbd "Q")     "exec sudo /etc/rc.d/mifo kill")
    (dk m (kbd "r")     "echo-mifo-random")
    (dk m (kbd "s")     "prompt-mifo-save")
    (dk m (kbd "S")     "exec mifo --stop")
    (dk m (kbd "t")     "exec mifo --toggle")
    (dk m (kbd "+")     "echo-mifo-fav-add")
    (dk m (kbd "-")     "echo-mifo-fav-del")
    (dk m (kbd "Return")"prompt-mifo-reload")
    (dk m (kbd "ESC")   "abort")
   M)))

;; mplayer daemon (mifo) useful seek commands.
(defvar *mplayer-map2*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    (dk m (kbd "h")     "exec mifo -c seek -7")
    (dk m (kbd "H")     "exec mifo -c seek -17")
    (dk m (kbd "C-h")   "exec mifo -c seek -47")
    (dk m (kbd "M-h")   "exec mifo -c seek -407")
    (dk m (kbd "l")     "exec mifo -c seek 5")
    (dk m (kbd "L")     "exec mifo -c seek 15")
    (dk m (kbd "C-l")   "exec mifo -c seek 45")
    (dk m (kbd "M-l")   "exec mifo -c seek 405")
    (dk m (kbd "!")     "exec mifo -c seek_chapter -1")
    (dk m (kbd "@")     "exec mifo -c seek_chapter 1")
    (dk m (kbd "BackSpace") "exec mifo -c seek 0 1")
    (dk m (kbd "ESC")   "abort")
   M)))

(setf *top-map*
  (let ((m (make-sparse-keymap)))
    (labels ((dk (m k c) (define-key m k c)))
    ;; <numerical bindings>
    (dk m (kbd "s-1")    "gselect 1")
    (dk m (kbd "s-2")    "gselect 2")
    (dk m (kbd "s-3")    "gselect 3")
    (dk m (kbd "s-4")    "gselect 4")
    (dk m (kbd "s-5")    "gselect 5")
    (dk m (kbd "s-6")    "gselect 6")
    (dk m (kbd "s-8")    "mark")
    (dk m (kbd "s-C-8")  "clear-window-marks")
    (dk m (kbd "s-M-8")  "gmove-marked")
    (dk m (kbd "s-9")    "quiet-resize -10 0")
    (dk m (kbd "s-0")    "quiet-resize  10 0")
    (dk m (kbd "C-1")    "select-window-by-number 1")
    (dk m (kbd "C-2")    "select-window-by-number 2")
    (dk m (kbd "C-3")    "select-window-by-number 3")
    (dk m (kbd "C-4")    "select-window-by-number 4")
    (dk m (kbd "C-5")    "select-window-by-number 5")
    (dk m (kbd "C-6")    "select-window-by-number 6")
    (dk m (kbd "C-7")    "select-window-by-number 7")
    (dk m (kbd "C-8")    "select-window-by-number 8")
    (dk m (kbd "C-9")    "select-window-by-number 9")
    (dk m (kbd "C-0")    "select-window-by-number 0")
    ;; <special-char bindings>
    (dk m (kbd "s-!")    "gmove 1")
    (dk m (kbd "s-@")    "gmove 2")
    (dk m (kbd "s-#")    "gmove 3")
    (dk m (kbd "s-$")    "gmove 4")
    (dk m (kbd "s-%")    "gmove 5")
    (dk m (kbd "s-^")    "gmove 6")
    (dk m (kbd "s-*")    "pull-marked")
    (dk m (kbd "s-(")    "quiet-resize 0 -10")
    (dk m (kbd "s-)")    "quiet-resize 0  10")
    (dk m (kbd "s--")    "vsplit")
    (dk m (kbd "s-=")    "hsplit")
    (dk m (kbd "s-+")    "balance-frames")
    (dk m (kbd "s-;")    "colon")
    (dk m (kbd "s-:")    "manpage")
    (dk m (kbd "s-C-;")  "eval")
    (dk m (kbd "s-,")    "gprev")
    (dk m (kbd "s-<")    "gmove-prev")
    (dk m (kbd "s-C-,")  "gprev-with-window")
    (dk m (kbd "s-.")    "gnext")
    (dk m (kbd "s->")    "gmove-next")
    (dk m (kbd "s-C-.")  "gnext-with-window")
    (dk m (kbd "s-/")    "gother")
    (dk m (kbd "s-?")    "lastmsg")
    (dk m (kbd "s-ESC")  "exec banishmouse")
    (dk m (kbd "s-Tab")  "fother")
    (dk m (kbd "s-S-SPC")"rotate-windows")
    (dk m (kbd "s-BackSpace")       "fclear")
    (dk m (kbd "s-S-BackSpace")     "delete-window")
    (dk m (kbd "s-C-BackSpace")     "kill-window")
    (dk m (kbd "s-Return")          "exec urxvt -e tmux -S /tmp/.${UID}/tmux/xorg new-session")
    (dk m (kbd "s-S-Return")        "tmux-attach-else-new")
    (dk m (kbd "s-C-Return")        "exec urxvt")
    (dk m (kbd "s-M-Return")        "tmux-attach-rtorrent")
    (dk m (kbd "s-SunPrint_Screen") "exec import -window root ${XDG_PICTURES_DIR:-${H:-/howl}/foto}/shot/$(date +%Y_%m_%d-%H%M%S).png")
    (dk m (kbd "C-M-Delete")        "exec alock -bg image:file=${XDG_PICTURES_DIR:-${H:-/howl}/foto}/wall/beheading.png -cursor glyph -auth pam >&/dev/null")
    (dk m (kbd "C-s-Delete")        "exec alock -bg image:file=${XDG_PICTURES_DIR:-${H:-/howl}/foto}/wall/beheading.png -cursor glyph -auth pam >&/dev/null")
    ;; <alphabetic bindings>
    (dk m (kbd "s-a")    *echo-map*)
    (dk m (kbd "s-b")    "refresh")
    (dk m (kbd "s-B")    "redisplay")
    (dk m (kbd "s-c")    *xclip-primary-map*)
    (dk m (kbd "s-C")    *xclip-clipboard-map*)
    (dk m (kbd "s-d")    *mplayer-map1*)
    (dk m (kbd "s-D")    "prompt-mifo-command")
    (dk m (kbd "s-e")    "exec ")
    (dk m (kbd "s-E")    "shell-command-output")
    (dk m (kbd "s-f")    *frequent-map*)
    (dk m (kbd "s-F")    *win-frame-map*)
    (dk m (kbd "s-g")    "vgroups")
    (dk m (kbd "s-G")    "grouplist")
    (dk m (kbd "s-h")    "move-focus left")
    (dk m (kbd "s-H")    "move-window left")
    (dk m (kbd "s-C-h")  "exchange-direction left")
    (dk m (kbd "s-M-h")  "exchange-direction-remain left")
    (dk m (kbd "s-i")    "fselect")
    (dk m (kbd "s-j")    "move-focus down")
    (dk m (kbd "s-J")    "move-window down")
    (dk m (kbd "s-C-j")  "exchange-direction down")
    (dk m (kbd "s-M-j")  "exchange-direction-remain down")
    (dk m (kbd "s-k")    "move-focus up")
    (dk m (kbd "s-K")    "move-window up")
    (dk m (kbd "s-C-k")  "exchange-direction up")
    (dk m (kbd "s-M-k")  "exchange-direction-remain up")
    (dk m (kbd "s-l")    "move-focus right")
    (dk m (kbd "s-L")    "move-window right")
    (dk m (kbd "s-C-l")  "exchange-direction right")
    (dk m (kbd "s-M-l")  "exchange-direction-remain right")
    (dk m (kbd "s-m")    "master-focus")
    (dk m (kbd "s-M")    "master-swap 0")
    (dk m (kbd "s-C-m")  "master-make")
    (dk m (kbd "s-n")    "next-in-frame")
    (dk m (kbd "s-N")    "pull-hidden-next")
    (dk m (kbd "s-o")    "fullscreen") 
    (dk m (kbd "s-O")    "only") 
    (dk m (kbd "s-p")    "prev-in-frame")
    (dk m (kbd "s-P")    "pull-hidden-previous")
    (dk m (kbd "s-Q")    "quit")
    (dk m (kbd "s-r")    "loadrc")
    (dk m (kbd "s-R")    "restart")
    (dk m (kbd "s-s")    *mplayer-map2*)
    (dk m (kbd "s-t")    "title")
    (dk m (kbd "s-u")    "undo")
    (dk m (kbd "s-v")    "show-window-properties")
    (dk m (kbd "s-V")    "list-window-properties")
    (dk m (kbd "s-w")    "echo-frame-windows")
    (dk m (kbd "s-W")    "windowlist")
    (dk m (kbd "s-x")    *xsel-map*)
    (dk m (kbd "s-y")    "iresize")
    (dk m (kbd "s-z")    "remove-split")
    ;; <function-key bindings>
    (dk m (kbd "XF86AudioMute")         "echo-oss-volmute")
    (dk m (kbd "XF86AudioRaiseVolume")  "echo-oss-volup")
    (dk m (kbd "XF86AudioLowerVolume")  "echo-oss-voldown")
    (dk m (kbd "s-C-F9")  "dump-to-datadir rules")
    (dk m (kbd "s-C-F10") "dump-to-datadir desktop")
    (dk m (kbd "s-C-F11") "dump-to-datadir screen")
    (dk m (kbd "s-C-F12") "dump-to-datadir group")
    (dk m (kbd "s-F9")    "restore-from-datadir rules")
    (dk m (kbd "s-F10")   "restore-from-datadir desktop")
    (dk m (kbd "s-F11")   "restore-from-datadir screen")
    (dk m (kbd "s-F12")   "restore-from-datadir group")
    (dk m (kbd "s-quoteleft") "scratchpad")
   M)))

;; EOF