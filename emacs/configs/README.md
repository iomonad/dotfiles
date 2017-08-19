# Configurations folder
| Folder        | Desc           | Enabled  |
| ------------- |:-------------:| -----:|
| core | emacs core configurations  | yes |
| package  | packages configurations    | yes |
| languages | Languages specific configurations | yes |
| experimental  | experimental configurations| no |
# Plugin template for packages configurations
```lisp
;; File: pkg-packagename.el
(use-package {{pkgname}} :ensure t
  :diminish {{modename}}
    :init {{init-function}})

(provide 'pkg-packagename)
```