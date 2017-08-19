;; Filename: lang-c++.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/23/2017 Sunday 17:58:27
;; Description: C++ configurations

(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(provide 'lang-c++)
