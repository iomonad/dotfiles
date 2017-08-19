" Vim filetype plugin file
" Language:	Gentoo Ebuilds / Eclasses
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" This sets up filetype specific options for Gentoo ebuilds and eclasses. Most
" of these settings are mandated by policy.xml. Some settings are inherited
" from vim's sh.vim. UTF-8 is selected as per GLEP 31.
"

if &compatible || v:version < 603
    finish
endif

runtime! ftplugin/sh.vim

" Include - in keyword characters (for strip-flags etc)
exec "setlocal iskeyword=" . &iskeyword . ",45"

" Required whitespace settings
setlocal tabstop=4
setlocal shiftwidth=4
setlocal noexpandtab

" GLEP 31 settings
setlocal fileencoding=utf-8
