" Title:    twof
" Creater:  4drift (http://github.com/m3atatarian)
" Credits:  Mustang.vim modified into busybee.vim then this
" ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "twof"

" Vim >= 7.0 specific colors
" ━━━━━━━━━━━━━━━━━━━━━━━━━━
if version >= 700
"hi CursorLine      ctermfg=252
hi MatchParen       ctermfg=255       ctermbg=237   cterm=bold
hi CursorLine       ctermbg=8         cterm=bold
hi CursorLineNr     ctermfg=9         ctermbg=235
hi CursorColumn     ctermbg=237
hi ColorColumn      cterm=bold        ctermbg=8
hi Pmenu            ctermfg=15        ctermbg=233
hi PmenuSel         ctermfg=15        ctermbg=1
hi PmenuSbar        ctermfg=252       ctermbg=0
hi PmenuThumb       ctermfg=252       ctermbg=0
hi SignColumn       ctermfg=252       ctermbg=0
endif

" General colors
" ━━━━━━━━━━━━━━
hi Cursor           ctermbg=241
hi Normal           ctermfg=15
hi NonText          ctermfg=235
hi LineNr           ctermfg=237       ctermbg=235
hi StatusLine       ctermfg=15        ctermbg=0     cterm=none
hi StatusLineNC     ctermfg=236       ctermbg=0     cterm=none
hi VertSplit        ctermfg=237
hi Folded           ctermfg=248       ctermbg=0
hi Title            ctermfg=254       ctermbg=0     cterm=none
hi Visual           ctermfg=252       ctermbg=1
hi SpecialKey       ctermfg=244       ctermbg=236
hi TabLineFill      ctermfg=8         ctermbg=0     cterm=none
hi TabLine          ctermfg=7         ctermbg=0     cterm=none
hi TabLineSel       ctermfg=9         ctermbg=236   cterm=none
hi FoldColumn       ctermfg=9         ctermbg=0

" Diff add is purple, change is blue, and text is green
" ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
hi DiffAdd         ctermfg=60         ctermbg=236
hi DiffDelete      ctermfg=233        ctermbg=233
hi DiffChange      ctermfg=24         ctermbg=236
hi DiffText        ctermfg=65         ctermbg=236

" Spell checking highlighting
" ━━━━━━━━━━━━━━━━━━━━━━━━━━━
hi SpellBad        ctermfg=88         ctermbg=236
hi SpellCap        ctermfg=61         ctermbg=236
hi SpellRare       ctermfg=25         ctermbg=236
hi SpellLocal      ctermfg=65         ctermbg=236

" Syntax highlighting
" ━━━━━━━━━━━━━━━━━━━
hi Comment         cterm=none         ctermfg=240
hi WildMenu        ctermfg=9          ctermbg=236
hi Todo            ctermfg=9          ctermbg=236   cterm=none
hi Delimiter       ctermfg=9          cterm=bold
hi Search          ctermfg=255        ctermbg=238
hi Identifier      ctermfg=12
hi WarningMsg      ctermfg=9
hi Number          ctermfg=9
hi Boolean         ctermfg=2
hi String          ctermfg=2
hi Function        ctermfg=250
hi Type            ctermfg=12
hi Statement       ctermfg=5
hi Keyword         ctermfg=13
hi Character       ctermfg=4
hi Constant        ctermfg=13
hi Special         ctermfg=4
hi PreProc         ctermfg=249
hi Error           ctermfg=255
hi Exception       ctermfg=4
hi Operator        ctermfg=4
hi StorageClass    ctermfg=5
hi netrwDir        ctermfg=9
hi netrwExe        ctermfg=13
hi netrwSymLink    ctermfg=14
hi netrwMakefile   ctermfg=5
hi netrwCompress   ctermfg=2

" Code-specific colors
" ━━━━━━━━━━━━━━━━━━━━
hi pythonImport                 ctermfg=252
hi pythonExClass                ctermfg=6
hi pythonBuiltinFunc            ctermfg=10
hi pythonBuiltin                ctermfg=6
hi pythonBuiltinObj             ctermfg=6
hi pythonFunction               ctermfg=252
hi pythonDecorator              ctermfg=14
hi pythonInclude                ctermfg=252
hi pythonRun                    ctermfg=13
hi pythonCoding                 ctermfg=13
hi pythonBoolean                ctermfg=5
hi pythonDot                    ctermfg=5

hi vimMapModKey                 ctermfg=4
hi vimBracket                   ctermfg=9    cterm=bold
hi vimNotation                  ctermfg=249
hi vimFuncSID                   ctermfg=2
hi vimSetSep                    ctermfg=6
hi vimSep                       ctermfg=4
hi vimContinue                  ctermfg=6

hi markdownCode                 ctermfg=240
hi markdownListMarker           ctermfg=4    cterm=bold
hi markdownLinkTextDelimiter    ctermfg=4    cterm=bold
hi markdownLinkDelimiter        ctermfg=4    cterm=bold
hi markdownUrl                  ctermfg=5
hi markdownLinkText             ctermfg=10

hi javascriptParens             ctermfg=4    cterm=bold
hi javascriptMember             ctermfg=4
hi javascriptNumber             ctermfg=3

hi cDelimiter                   ctermfg=9    cterm=bold
hi cOperator                    ctermfg=250
hi cStructure                   ctermfg=4
hi cIncluded                    ctermfg=13
hi cDefine                      ctermfg=249
hi cUserFunction                ctermfg=2
hi cUserFunctionPointer         ctermfg=5

hi cssProp                      ctermfg=6
hi shOperator                   ctermfg=9    cterm=bold
hi htmlLink                     ctermfg=14

hi sxBrace                      ctermfg=9
hi sxKeysym                     ctermfg=6

" vim:ft=vim :
