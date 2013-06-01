let mapleader = ','

" Buffer mappings {{{
  nnoremap <silent> <Leader>d :bd<CR>
  nnoremap <silent> <Leader>c :clo<CR>
" }}}

" Sudo tee to write a file as root {{{
  cmap w!! w !sudo tee % >/dev/null
" }}}

" CTags {{{
  map <Leader>rt :!ctags --extra=+f -R *<CR><CR>
" }}}

" Quick edit commands {{{
  " NOTE This is overridden by CamelCaseMotion
  map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
  map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>
  map <Leader>ve :vsp <C-R>=expand("%:p:h") . "/" <CR>
  map <Leader>se :sp <C-R>=expand("%:p:h") . "/" <CR>
" }}}

" <C-L> clear highlight as well as redraw {{{
  nnoremap <C-L> :nohls<CR><C-L>
  inoremap <C-L> <C-O>:nohls<CR>
" }}}

" map Q to something useful {{{
  noremap Q gq
" }}}

" make Y consistent with C and D {{{
  nnoremap Y y$
" }}}

" jj to exit insert mode {{{
  imap jj <Esc>
" }}}

" Semi-colon to enter command mode {{{
  " NOTE semi-colon will repeat the last 'f' find. Space.vim provides this
  " same functionality so we can use semi-colon for something else.
  au VimEnter * noremap ; :
" }}}

" Search with more magic {{{
  " NOTE This is overridden by IndexedSearch
  nnoremap / /\v
  vnoremap / /\v
" }}}

" Go to column and row when looking up a bookmark by default {{{
  nnoremap ' `
  nnoremap ` '
" }}}

" Use enter to insert newlines in normal mode, but not in quickfix {{{
  function! s:insert_line(direction)
    if &buftype == "quickfix" || &buftype == "nofile"
      execute "normal! \<Enter>"
    else
      if a:direction == 'below'
        execute "normal! o\<Esc>"
      else
        execute "normal! O\<Esc>"
      endif
    endif
  endfunction

  nmap <Enter> :call <SID>insert_line('below')<CR>
  nmap <S-Enter> :call <SID>insert_line('above')<CR>
" }}}

" Bubbling lines {{{
  nmap <C-Up> [e
  nmap <C-Down> ]e
  vmap <C-Up> [egv
  vmap <C-Down> ]egv
" }}}

" Visual search {{{
  function! s:VSetSearch()
    let temp = @@
    norm! gvy
    let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
    let @@ = temp
  endfunction

  vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR>
  vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR>
" }}}

" Convert 1.8 style Ruby hashes to terser 1.9 style {{{
  nnoremap <Leader>hh :%s/\v:(\w{-}) \=\> /\1: /g<CR>''
" }}}

" Go to next/previous line with same indentation {{{
  " Jump to the next or previous line that has the same level or a lower
  " level of indentation than the current line.
  "
  " exclusive (bool): true: Motion is exclusive
  " false: Motion is inclusive
  " fwd (bool): true: Go to next line
  " false: Go to previous line
  " lowerlevel (bool): true: Go to line with lower indentation level
  " false: Go to line with the same indentation level
  " skipblanks (bool): true: Skip blank lines
  " false: Don't skip blank lines
  function! NextIndent(exclusive, fwd, lowerlevel, skipblanks)
    let line = line('.')
    let column = col('.')
    let lastline = line('$')
    let indent = indent(line)
    let stepvalue = a:fwd ? 1 : -1
    while (line > 0 && line <= lastline)
      let line = line + stepvalue
      if ( ! a:lowerlevel && indent(line) == indent ||
            \ a:lowerlevel && indent(line) < indent)
        if (! a:skipblanks || strlen(getline(line)) > 0)
          if (a:exclusive)
            let line = line - stepvalue
          endif
          exe line
          exe "normal " column . "|"
          return
        endif
      endif
    endwhile
  endfunction

  " Moving back and forth between lines of same or lower indentation.
  nnoremap <silent> <D-k> :call NextIndent(0, 0, 0, 1)<CR>
  nnoremap <silent> <D-j> :call NextIndent(0, 1, 0, 1)<CR>
  nnoremap <silent> <D-K> :call NextIndent(0, 0, 1, 1)<CR>
  nnoremap <silent> <D-J> :call NextIndent(0, 1, 1, 1)<CR>
  vnoremap <silent> <D-k> <Esc>:call NextIndent(0, 0, 0, 1)<CR>m'gv''
  vnoremap <silent> <D-j> <Esc>:call NextIndent(0, 1, 0, 1)<CR>m'gv''
  vnoremap <silent> <D-K> <Esc>:call NextIndent(0, 0, 1, 1)<CR>m'gv''
  vnoremap <silent> <D-J> <Esc>:call NextIndent(0, 1, 1, 1)<CR>m'gv''
  onoremap <silent> <D-k> :call NextIndent(0, 0, 0, 1)<CR>
  onoremap <silent> <D-j> :call NextIndent(0, 1, 0, 1)<CR>
  onoremap <silent> <D-K> :call NextIndent(1, 0, 1, 1)<CR>
  onoremap <silent> <D-J> :call NextIndent(1, 1, 1, 1)<CR>
" }}}
