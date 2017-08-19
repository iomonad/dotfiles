;; Filename: conf-keybinds.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 05/28/2017 Sunday 15:55:48
;; Description: Custom keybinds go here

;; Move Alt-x to Ctrl-X/Ctrl-M
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Toggle undotree diff tool
(global-set-key "\C-v" 'undo-tree-visualize)
(global-set-key (kbd "C-z") 'undo) ;; Alias to undotree
(global-set-key (kbd "C-S-z") 'redo)

;; NeoTree keybinds
(global-set-key [f8] 'neotree-toggle)

;; <C-c>  User defined keybinds
;; Translate keys
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

;; Other C-c based keybinds
(global-set-key "\C-ci" 'indent-region)

;; Show agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

;(global-set-key (kbd "C-c a") 'indent-full-buffer) ;; The lazy way

(provide 'conf-keybinds)
