" Vim indent file
" Language:		eRuby
" Maintainer:		Tim Pope <vimNOSPAM@tpope.info>
" Info:			$Id: eruby.vim,v 1.9 2007/04/16 17:03:36 tpope Exp $
" URL:			http://vim-ruby.rubyforge.org
" Anon CVS:		See above site
" Release Coordinator:	Doug Kearns <dougkearns@gmail.com>

if exists("b:did_indent")
  finish
endif

runtime! indent/ruby.vim
unlet! b:did_indent
set indentexpr=

if exists("b:eruby_subtype")
  exe "runtime! indent/".b:eruby_subtype.".vim"
else
  runtime! indent/html.vim
endif
unlet! b:did_indent

if &l:indentexpr == ''
  if &l:cindent
    let &l:indentexpr = 'cindent(v:lnum)'
  else
    let &l:indentexpr = 'indent(prevnonblank(v:lnum-1))'
  endif
endif
let b:eruby_subtype_indentexpr = &l:indentexpr

let b:did_indent = 1

setlocal indentexpr=GetErubyIndent()
setlocal indentkeys=o,O,*<Return>,<>>,{,},0),0],o,O,!^F,=end,=else,=elsif,=rescue,=ensure,=when

" Only define the function once.
if exists("*GetErubyIndent")
  finish
endif

function! GetErubyIndent()
  let vcol = col('.')
  call cursor(v:lnum,1)
  let inruby = searchpair('<%','','%>')
  call cursor(v:lnum,vcol)
  if inruby && getline(v:lnum) !~ '^<%'
    let ind = GetRubyIndent()
  else
    exe "let ind = ".b:eruby_subtype_indentexpr
  endif
  let lnum = prevnonblank(v:lnum-1)
  let line = getline(lnum)
  let cline = getline(v:lnum)
  if cline =~# '<%\s*\%(end\|else\|\%(ensure\|rescue\|elsif\|when\).\{-\}\)\s*\%(-\=%>\|$\)'
    let ind = ind - &sw
  endif
  if line =~# '\<do\%(\s*|[^|]*|\)\=\s*-\=%>'
    let ind = ind + &sw
  elseif line =~# '<%\s*\%(module\|class\|def\|if\|for\|while\|until\|else\|elsif\|case\|when\|unless\|begin\|ensure\|rescue\)\>.*%>'
    let ind = ind + &sw
  endif
  if line =~# '^\s*<%[=#]\=\s*$' && cline !~# '^\s*end\>'
    let ind = ind + &sw
  endif
  if cline =~# '^\s*-\=%>\s*$'
    let ind = ind - &sw
  endif
  return ind
endfunction

" vim:set sw=2 sts=2 ts=8 noet ff=unix:
