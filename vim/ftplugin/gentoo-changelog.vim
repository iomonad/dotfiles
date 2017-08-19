" Vim filetype plugin file
" Language:	Gentoo ChangeLogs
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" Sets up settings for Gentoo ChangeLogs as per GLEP 31.
"

if &compatible || v:version < 603
    finish
endif

" GLEP 31 settings
setlocal fileencoding=utf-8
