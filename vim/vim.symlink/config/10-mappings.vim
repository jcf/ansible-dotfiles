let mapleader = ','

" Buffer mappings {{{
  nnoremap <silent> <Leader>d :bd<CR>
" }}}

" Sudo tee to write a file as root {{{
  cmap w!! w !sudo tee % >/dev/null
" }}}

" CTags {{{
  map <Leader>rt :!ctags --extra=+f -R *<CR><CR>
" }}}

" Opens edit command with the path of buffer {{{
  " NOTE This is overridden by CamelCaseMotion
  map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" }}}

" Opens tabedit command with the path of buffer {{{
  map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>
" }}}

" Inserts the path of buffer in to command {{{
  " Command mode: Ctrl+P
  cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
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

" JJ to exit insert mode {{{
  imap jj <Esc>
" }}}

" Semi-colon to enter command mode {{{
  " NOTE semi-colon will repeat the last 'f' find. Space.vim provides this
  " same functionality so we can use semi-colon for something else.
  au VimEnter * noremap ; :
" }}}

" Search with more magic {{{
  nnoremap / /\v
  vnoremap / /\v
" }}}

" Go to column and row when looking up a bookmark by default {{{
  nnoremap ' `
  nnoremap ` '
" }}}

" Use enter to insert newlines in normal mode, but not in quickfix {{{
  function! s:insert_line(direction)
    if &buftype == "quickfix"
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
