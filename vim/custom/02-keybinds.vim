" File: 02-keybinds.vim
" Author: Clement Trosa <me@trosa.io>
" Date: 18/06/2017 06:28:41 PM
" Last Modified: 18/06/2017 06:28:41 PM

" Core {{{
map <C-x><C-n> :xall<CR>          " emacs alias
cmap W w                          " Faster exit alias
cmap qq qa!                       " ^ same
nmap <leader>w :w!<CR>            " avoid last step message
nmap <leader>cl :left<CR>         " \
nmap <leader>cr :right<CR>        " x> text alignement
nmap <leader>cc :center<CR>       " /
vnoremap S :sort<CR>              " usefull sorting
nnoremap <C-n> :bnext<CR>
nnoremap <C-p> :bprev<CR>
nnoremap <leader>b :buffers<CR>:buffer<space>
nnoremap <leader>l :ls<CR>
nnoremap <leader>g :e#<CR>
cnoremap  <c-a>   <left>
cnoremap  <c-e>   <right>

" Buffer move
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>


" }}}

" Nerdtree Toggle {{{
map <C-f> :NERDTreeToggle<CR>
let NERDTreeChDirMode = 2
let NERDTreeShowBookmarks = 1
" }}}
" Gundo toggle {{{
nnoremap <C-g> :GundoToggle<CR>
" }}}

" Goyo leader mapping {{{
nmap <leader>g :Goyo<CR>
" }}}

" Headers leader shortcuts {{{
nmap <leader>hh :AddHeader<CR>
nmap <leader>ha :AddApacheLicense<CR>
nmap <leader>hg :AddGNULicense<CR>
nmap <leader>hm :AddMITLicense<CR>
" }}}
