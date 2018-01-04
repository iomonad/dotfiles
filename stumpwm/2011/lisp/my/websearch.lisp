
(defmacro make-web-jump (name url-prefix)
  `(defcommand ,name (search)
               ((:rest ,(concatenate 'string (symbol-name name) ": ")))
               "Search web"
               (run-shell-command (format nil "firefox '~A'"
                                          (concat ,url-prefix (substitute #\+ #\Space search))))))

(make-web-jump imdb      "http://www.imdb.com/find?q=")
(make-web-jump google    "http://www.google.com/search?q=")
(make-web-jump wikipedia "http://en.wikipedia.org/wiki/Special:Search?fulltext=Search&search=")
(make-web-jump youtube   "http://youtube.com/results?search_query=")
(make-web-jump bbs       "http://bbs.archlinux.org/search.php?action=search&show_as=topics&sort_dir=DESC&keywords=")
(make-web-jump bbsa      "http://bbs.archlinux.org/search.php?action=search&show_as=topics&sort_dir=DESC&author=")
(make-web-jump awiki     "https://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=")
(make-web-jump pac       "http://www.archlinux.org/packages/?q=")
(make-web-jump aur       "http://aur.archlinux.org/packages.php?K=")
(make-web-jump last.fm   "http://www.last.fm/search?q=")
(make-web-jump port      "http://crux.nu/portdb/?a=search&q=")
