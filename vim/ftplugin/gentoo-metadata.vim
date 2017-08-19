" Vim filetype plugin file
" Language:	Gentoo metadata.xml
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" This sets up filetype specific options for Gentoo metadata.xml files.
"

if &compatible || v:version < 603
    finish
endif

runtime! ftplugin/xml.vim

" Required whitespace settings
setlocal tabstop=4
setlocal shiftwidth=4
setlocal noexpandtab

" GLEP 31 settings
setlocal fileencoding=utf-8
