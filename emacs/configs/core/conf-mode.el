;; Filename: conf-mode.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/09/2017 Sunday 22:50:43
;; Description: Set mode for a default sessions

(mapc (lambda (mode) (funcall mode 1))
      '(auto-compression-mode
        column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        ido-mode
        line-number-mode
        show-paren-mode
        subword-mode
        transient-mark-mode
        which-function-mode))

(provide 'conf-mode)
