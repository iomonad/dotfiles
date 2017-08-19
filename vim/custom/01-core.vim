
" Author: Clement Trosa <me@trosa.io>
" Date: 27/06/2017 10:32:11 AM


" Author: Clement Trosa <me@trosa.io>
" Date: 18/06/2017 06:27:40 PM


" General settings {{{
 filetype on
 filetype plugin indent on
 syntax on
 colorscheme desert

 " Globals {{{
 set nocp              " no compatible to vi
 set encoding=utf-8    " 2017 ?
 set fileformats=unix  " gentoo ftw
 set exrc              " let reading vimrc from current directory
 set title             " remove program title in terminal
 set mousehide         " pleb mode
 set autoread          " automatically read updated file again.
 set autochdir	       " vim will change the current working directory whenever you open file
 set autoindent        " smart code indent
 set visualbell        " just for feedback
 set cursorline        " make cursor more visible
 set ruler             " Always show cursor

 let mapleader = ","   " default key leader (second mapping)
 set shortmess=at      " shorten error messages
 set modeline          " enable modelines
 set fillchars=""      " this is so ugly.
 set modelines=5       " gives the number of lines that is checked for set commands
 set number            " enable line numbers
 set ruler             " enable something
 set cursorline        " enable hiliting of cursor line
 set backspace=2       " backspace over EOL etc.
 set background=dark   " just a preference
 set hidden            " buffer switching should be quick
 set confirm           " ask instead of just print errors
 set equalalways       " make splits equal size
 set lazyredraw        " don't redraw while executing macros
 set noshowmode        " don't display mode, it's already in the status line
 set hlsearch	         " highlight researchs
 set tabstop=2         " number of spaces that a <Tab> in the file counts for
 set shiftwidth=2      " number of spaces to use for each step of (auto)indent
 set softtabstop=2     " number of spaces that a <Tab> counts for while performing editing operations
 set smartindent       " do smart autoindenting when starting a new line.
 set expandtab         " use the appropriate number of spaces to insert a <Tab>
 set incsearch	       " while typing a search command, show where the pattern, as it was typed so far, matches.
 set ignorecase        " ignore case in search patterns
 set nobackup          " no backups
 set nowb	             " no write backups
 set noswapfile        " no fucking crap files
 set wildmenu          " command-line completion operates in an enhanced mode.
 set wildmode=longest,full,list
 set wildignore=*.swp,*.bak,*~,blib,*.o,*.png,*.jpe?g,.git,.svn,*.so,.hg,*.exe
 set history=50        " a history of ":" commands
 set helplang=fr,nl    " use my languages for documentation
 "set paste             " use paste for xorg server (buggy)
 set relativenumber    " show the line number relative to the line with the cursor in front
 set numberwidth=4     " set space between number and buffer
 " }}}

 " Rest of configs {{{
 set backspace=indent,eol,start " allow backspacing over everything in insert mode
 set backspace=start,indent,eol
 set clipboard=unnamedplus,autoselect
 set complete=.,w,b,u,t,i,d,k,kspell
 set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
 " }}}
" }}}

" Folding {{{
  set foldmethod=marker
  set foldcolumn=1
  set showmatch
" }}}

" Execute at exit {{{
autocmd BufWritePre * silent! %s/\s\+$//e " Autoremove trailling character
" autocmd BufNewFile  * silent! :write " write on creation
"autocmd BufWritePre * silent! :AddHeader " Update date if no headers
"}}}
