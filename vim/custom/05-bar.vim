" File: custom/05-bar.vim
" Author: Clement Trosa <me@trosa.io>
" Date: 18/06/2017 10:51:10 PM
" Last Modified: 18/06/2017 10:51:10 PM

" Status line {{{
  set laststatus=2      " always show statusline

  " Generic Statusline {{{
  function! SetStatus()
    setl statusline+=
          \%1*\ %f
          \%H%M%R%W%7*\ ❙
          \%2*\ %Y\ <<<\ %{&ff}%7*\ ❙
  endfunction

  function! SetRightStatus()
    setl statusline+=
          \%5*\ %{StatusFileencoding()}%7*\ ~
          \%5*\ %{StatusBuffersize()}%7*\ ❙
          \%=%<%7*\ ❙
          \%5*\ %{StatusWrapON()}
          \%6*%{StatusWrapOFF()}\ %7*\❙
          \%5*\ %{StatusInvisiblesON()}
          \%6*%{StatusInvisiblesOFF()}\ %7*\~\
          \%5*\%{StatusExpandtabON()}
          \%6*\ %{StatusExpandtabOFF()}\ %7*
          \%5*\❙\ w%{StatusTabstop()}\%7*\ ~\
          \%3*\ %l,%c\ >>>\ {\%P\}
  endfunction " }}}

  " Update when leaving Buffer {{{
  function! SetStatusLeaveBuffer()
    setl statusline=""
    call SetStatus()
  endfunction
  au BufLeave * call SetStatusLeaveBuffer() " }}}

  " Update when switching mode {{{
  function! SetStatusInsertMode(mode)
    setl statusline=%4*
    if a:mode == 'i'
      setl statusline+=\ Invoeging\ ◥
    elseif a:mode == 'v'
      setl statusline+=\ Visuele\ ◥
    elseif a:mode == 'normal'
      setl statusline+=\ \ λ
    endif
    call SetStatus()
    call SetRightStatus()
  endfunction

  au VimEnter     * call SetStatusInsertMode('normal')
  au InsertEnter  * call SetStatusInsertMode(v:insertmode)
  au InsertLeave  * call SetStatusInsertMode('normal')
  au BufEnter     * call SetStatusInsertMode('normal') " }}}

  " Some Functions shamelessly ripped and modified from Cream
  "fileencoding (three characters only) {{{
  function! StatusFileencoding()
    if &fileencoding == ""
      if &encoding != ""
        return &encoding
      else
        return " -- "
      endif
    else
      return &fileencoding
    endif
  endfunc " }}}
  " &expandtab {{{
  function! StatusExpandtabON()
    if &expandtab == 0
      return "tabs"
    else
      return ""
    endif
  endfunction "
  function! StatusExpandtabOFF()
    if &expandtab == 0
      return ""
    else
      return "tabs"
    endif
  endfunction " }}}
  " tabstop and softtabstop {{{
  function! StatusTabstop()

    " show by Vim option, not Cream global (modelines)
    let str = "" . &tabstop
    " show softtabstop or shiftwidth if not equal tabstop
    if   (&softtabstop && (&softtabstop != &tabstop))
    \ || (&shiftwidth  && (&shiftwidth  != &tabstop))
      if &softtabstop
        let str = str . ":sts" . &softtabstop
      endif
      if &shiftwidth != &tabstop
        let str = str . ":sw" . &shiftwidth
      endif
    endif
    return str

  endfunction " }}}
  " Buffer Size {{{
  function! StatusBuffersize()
    let bufsize = line2byte(line("$") + 1) - 1
    " prevent negative numbers (non-existant buffers)
    if bufsize < 0
      let bufsize = 0
    endif
    " add commas
    let remain = bufsize
    let bufsize = ""
    while strlen(remain) > 3
      let bufsize = "," . strpart(remain, strlen(remain) - 3) . bufsize
      let remain = strpart(remain, 0, strlen(remain) - 3)
    endwhile
    let bufsize = remain . bufsize
    " too bad we can't use "¿" (nr2char(1068)) :)
    let char = "b"
    return bufsize . char
  endfunction " }}}
  " Show Invisibles {{{
  function! StatusInvisiblesON()
    "if exists("g:LIST") && g:LIST == 1
    if &list
      if     &encoding == "latin1"
        return "¶"
      elseif &encoding == "utf-8"
        return "¶"
      else
        return "$"
      endif
    else
      return ""
    endif
  endfunction
  function! StatusInvisiblesOFF()
    "if exists("g:LIST") && g:LIST == 1
    if &list
      return ""
    else
      if     &encoding == "latin1"
        return "¶"
      elseif &encoding == "utf-8"
        return "¶"
      else
        return "$"
      endif
    endif
  endfunction " }}}
  " Wrap Enabled {{{
  function! StatusWrapON()
    if &wrap
      return "wrap"
    else
      return ""
    endif
  endfunction
  function! StatusWrapOFF()
    if &wrap
      return ""
    else
      return "wrap"
    endif
  endfunction
  " }}}
" }}}
