;; Filename: conf-filetypes.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 04/09/2017 Sunday 23:23:39
;; Description: Specify mode depend of the file extension

(setq auto-mode-alist
      (nconc
       '(("COMMIT_EDITMSG$"  . diff-mode))
       '(("\.fsh$"           . c++-mode))
       '(("\.vsh$"           . c++-mode))
       '(("\.lua$"           . lua-mode))
       '(("\.md$"            . markdown-mode))
       '(("\.markdown$"      . markdown-mode))
       '(("\.xml$"           . nxml-mode))
       '(("\.html$"          . nxml-mode))
       '(("\.coffee$"        . coffee-mode))
       '(("\.rb$"            . ruby-mode))
       '(("\.hbs$"           . mustache-mode))
       '(("\.m$"             . objc-mode))
       '(("\.haml$"          . haml-mode))
       '(("\.scss$"          . css-mode))
       '(("\.yml$"           . yaml-mode))
       '(("\.yaml$"          . yaml-mode))
       '(("\.json$"          . yaml-mode))
       '(("\.mustache$"      . mustache-mode))
       '(("\.rb$"            . ruby-mode))
       '(("\.gemspec$"       . ruby-mode))
       '(("\.clj$"           . clojure-mode))
       '(("\.md$"            . markdown-mode))
       '(("\.textile$"       . textile-mode))
       '(("\.zsh$"           . sh-mode))
       '(("\.sass$"          . sass-mode))
       '(("\.js$"            . js2-mode))
       '(("\.js.erb$"        . js2-mode))
       '(("\.j$"             . objj-mode))
       '("\.rs$"             . rust-mode)
       '(("\.rake$"          . ruby-mode))
       '(("Gemfile$"         . ruby-mode))
       '(("Rakefile$"        . ruby-mode))
       '(("Dockerfile"       . dockerfile-mode))
       '(("\.js\\.erb$"      . javascript-mode))
       '(("\.coffee\\.erb$"  . coffee-mode))
       auto-mode-alist))

(provide 'conf-filetypes)
