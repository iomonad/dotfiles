;; Filename: conf-macros.el
;; Copyright (c) 2008-2017 Clement Trösa <iomonad@riseup.net>
;; 
;; Last-Updated: 06/04/2017 Sunday 11:54:59
;; Description: Some hacks to speedup productivity

;; Select and indent full buffer
(fset 'indent-full-buffer
      "\C-xh\C-[xinden-\C-?\C-?t\C-?nt-reg\C-m")

;; Insert apache copyright
(fset 'apache-copyright
      "/*\C-m * Copyright (c) 2017 iomonad <iomonad@riseup.net>.\C-m *\C-m * Licensed under the Apache License, Version 2.0 (the \"License\");\C-m * you may not use this file except in compliance with the License.\C-m * You may obtain a copy of the License at\C-m *\C-m * http://www.apache.org/licenses/LICENSE-2.0\C-m *\C-m * Unless required by applicable law or agreed to in writing, software\C-m * distributed under the License is distributed on an \"AS IS\" BASIS,\C-m * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\C-m * See the License for the specific language governing permissions and\C-m * limitations under the License.\C-m */")

(provide 'conf-macros)
