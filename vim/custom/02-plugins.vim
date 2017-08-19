" File: 02-plugins.vim
" Author: Clement Trosa <me@trosa.io>
" Date: 18/06/2017 06:26:46 PM
" Last Modified: 27/06/2017 09:25:22 PM
" Let's use Vundle {{{
  filetype off
  call plug#begin('~/.vim/plugged')
  "
  " github repos
  Plug 'vim-scripts/Gundo'
  Plug 'trapd00r/x11colors.vim'
  Plug 'pbondoer/vim-42header'
  Plug 'scrooloose/nerdtree'
  Plug 'alpertuna/vim-header'
  Plug 'scrooloose/nerdcommenter'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-syntastic/syntastic'
  Plug 'terryma/vim-multiple-cursors'
  Plug 'Shougo/neocomplete.vim'
  Plug 'honza/vim-snippets'
  Plug 'Shougo/neosnippet'
  Plug 'Shougo/neosnippet-snippets'
  Plug 'junegunn/limelight.vim'
  Plug 'Raimondi/delimitMate'
  Plug 'junegunn/goyo.vim'
  Plug 'kien/rainbow_parentheses.vim'
  " vim-scripts repos
  " non github repos
  call plug#end()            " required
" }}}
