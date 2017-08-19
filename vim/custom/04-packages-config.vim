" File: 04-packages-config.vim
" Author: Clement Trosa <me@trosa.io>
" Date: 18/06/2017 06:26:32 PM
" Last Modified: 18/06/2017 06:26:32 PM

" Nerdtree configuration {{{
let g:NERDTreeDirArrowExpandable = '~'
let g:NERDTreeDirArrowCollapsible = '-'
" }}}

" NerdCommenter {{{
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDAltDelims_java = 1
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
" }}}

" Gundo Configuration {{{
let g:gundo_width = 60
let g:gundo_preview_height = 30 " Set the vertical height of the Gundo preview.
let g:gundo_right = 1
let g:gundo_preview_bottom = 0 " Preview window more space to show the unified diff.
let g:gundo_help = 0
let g:gundo_map_move_older = "j" " Navigation bindings
let g:gundo_map_move_newer = "k"
let g:gundo_close_on_revert = 1 " Optimize ui
let g:gundo_auto_preview = 1
" }}}

" Auto Vim headers {{{
let g:header_field_author = user_name
let g:header_field_author_email = user_mail
let g:header_field_timestamp = 1
let g:header_field_timestamp_format = '%d/%m/%Y %r'
let g:header_auto_add_header = 1
let g:header_field_modified_timestamp = 1
" }}}

" Syntastic {{{
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 1
"}}}

" " You Completeme {{{
" let g:ycm_min_num_of_chars_for_completion = 2 " like emacs
" let g:ycm_min_num_identifier_candidate_chars = 0
" let g:ycm_auto_trigger = 1
" let g:ycm_filetype_whitelist = { '*': 1 }
" let g:ycm_filetype_blacklist = {
"       \ 'tagbar' : 1,
"       \ 'qf' : 1,
"       \ 'notes' : 1,
"       \ 'markdown' : 1,
"       \ 'unite' : 1,
"       \ 'text' : 1,
"       \ 'vimwiki' : 1,
"       \ 'pandoc' : 1,
"       \ 'infolog' : 1,
"       \ 'mail' : 1
"       \}
" let g:ycm_filetype_specific_completion_to_disable = {
"       \ 'gitcommit': 1
"       \}
" let g:ycm_show_diagnostics_ui = 1
" let g:ycm_error_symbol = '>>'
" let g:ycm_warning_symbol = '~>'
" let g:ycm_enable_diagnostic_signs = 1
" let g:ycm_enable_diagnostic_highlighting = 1
" let g:ycm_echo_current_diagnostic = 1
" let g:ycm_filter_diagnostics = {
"   \ "java": {
"   \      "regex": [ ".*taco.*"],
"   \      "level": "error"
"   \    }
"   \ }
" let g:ycm_always_populate_location_list = 0
" let g:ycm_open_loclist_on_ycm_diags = 1
" let g:ycm_complete_in_comments = 0
" let g:ycm_complete_in_strings = 1
" let g:ycm_collect_identifiers_from_comments_and_strings = 0
" let g:ycm_collect_identifiers_from_tags_files = 0
" let g:ycm_seed_identifiers_with_syntax = 0
" let g:ycm_extra_conf_vim_data = []
" let g:ycm_server_python_interpreter = ''
" let g:ycm_keep_logfiles = 0 " pls
" let g:ycm_log_level = 'info'
" let g:ycm_auto_start_csharp_server = 0 " nope
" let g:ycm_auto_stop_csharp_server = 1
" let g:ycm_csharp_server_port = 0
" let g:ycm_add_preview_to_completeopt = 0
" let g:ycm_autoclose_preview_window_after_completion = 0
" let g:ycm_autoclose_preview_window_after_insertion = 0
" let g:ycm_max_diagnostics_to_display = 30
" let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']
" let g:ycm_key_list_previous_completion = ['<S-TAB>', '<Up>']
" let g:ycm_key_invoke_completion = '<C-Space>'
" let g:ycm_key_detailed_diagnostics = '<leader>d'
" let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
" let g:ycm_confirm_extra_conf = 1
" let g:ycm_extra_conf_globlist = ['~/dev/*','!~/*']
" let g:ycm_extra_conf_globlist = []
" let g:ycm_filepath_completion_use_working_dir = 0
" let g:ycm_semantic_triggers =  {
"   \   'c' : ['->', '.'],
"   \   'objc' : ['->', '.', 're!\[[_a-zA-Z]+\w*\s', 're!^\s*[^\W\d]\w*\s',
"   \             're!\[.*\]\s'],
"   \   'ocaml' : ['.', '#'],
"   \   'cpp,objcpp' : ['->', '.', '::'],
"   \   'perl' : ['->'],
"   \   'php' : ['->', '::'],
"   \   'cs,java,javascript,typescript,d,python,perl6,scala,vb,elixir,go' : ['.'],
"   \   'ruby' : ['.', '::'],
"   \   'lua' : ['.', ':'],
"   \   'erlang' : [':'],
"   \ }
" let g:ycm_cache_omnifunc = 1
" let g:ycm_use_ultisnips_completer = 1
" let g:ycm_goto_buffer_command = 'same-buffer'
" let g:ycm_disable_for_files_larger_than_kb = 1000
" let g:ycm_python_binary_path = 'python'
"
" augroup load_ycm
"   autocmd!
"   autocmd CursorHold, CursorHoldI * :packadd YouCompleteMe
"                                 \ | autocmd! load_ycm
" augroup END
" " }}}

" Neocomplete {{{
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
" }}}

" Code Snippets {{{
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
" imap <C-k>     <Plug>(neosnippet_expand_or_jump)
imap <TAB>     <Plug>(neosnippet_expand_or_jump)
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif
" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
" }}}

" Limelight {{{
nmap <Leader>l <Plug>(Limelight)
xmap <Leader>l <Plug>(Limelight)
" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240

" Color name (:help gui-colors) or RGB color
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'

" Default: 0.5
let g:limelight_default_coefficient = 0.7

" Number of preceding/following paragraphs to include (default: 0)
let g:limelight_paragraph_span = 1

" Beginning/end of paragraph
"   When there's no empty line between the paragraphs
"   and each paragraph starts with indentation
let g:limelight_bop = '^\s'
let g:limelight_eop = '\ze\n^\s'

" Highlighting priority (default: 10)
"   Set it to -1 not to overrule hlsearch
let g:limelight_priority = -1
" }}}

" Multiple cursor {{{
let g:multi_cursor_use_default_mapping=0

" }}}

" Rainbow parenthesis {{{
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
" }}}
