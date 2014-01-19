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
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" }}}

" <C-L> clear highlight as well as redraw {{{
nnoremap <C-;> :nohls<CR><C-L>
inoremap <C-;> <C-O>:nohls<CR>
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
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
nnoremap :s/ :s/\v
" }}}

" Command-line window {{{
nnoremap q: q:i
nnoremap q/ q/i
nnoremap q? q?i
" }}}

" Folds {{{
nnoremap zr zr:echo &foldlevel<cr>
nnoremap zm zm:echo &foldlevel<cr>
nnoremap zR zR:echo &foldlevel<cr>
nnoremap zM zM:echo &foldlevel<cr>
" }}}

" Auto center next match {{{
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz
nnoremap <silent> <C-o> <C-o>zz
nnoremap <silent> <C-i> <C-i>zz
"}}}

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

" Tabular.vim {{{
nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
nmap <Leader>a: :Tabularize /:\zs<CR>
vmap <Leader>a: :Tabularize /:\zs<CR>
nmap <Leader>a, :Tabularize /,<CR>
vmap <Leader>a, :Tabularize /,<CR>
nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
" }}}

" Fugitive {{{
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gc :Gcommit -v<CR>
nmap <Leader>gw :Gwrite<CR>
nmap <Leader>gl :Gblame<CR>
nmap <Leader>gb :Gbrowse<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gg :Ggrep<Space>
" }}}

" Gitv {{{
nmap <Leader>gv :Gitv<CR>
" }}}

" NeoComplete {{{
" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>

function! s:my_cr_function()
  return neocomplete#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction

" <TAB>: completion.
" inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplete#close_popup()
inoremap <expr><C-e> neocomplete#cancel_popup()
" }}}

" PickHEX {{{
imap <D-C> <c-o>:PickHEX<CR>
nmap <D-C> :PickHEX<CR>
" }}}

" Tube.vim {{{
let g:tube_terminal = 'iterm' " Or Terminal if you prefer

function! TubeThis(...) abort
  let l:cmd = []
  let l:path = expand('%')

  if filewritable('.zeus.sock')
    call add(l:cmd, 'zeus')
  elseif filereadable('project.clj')
    call add(l:cmd, 'lein with-profile test')
  elseif filereadable('Gemfile')
    call add(l:cmd, 'bundle exec')
  endif

  if l:path =~# '_spec\.rb$'
    let l:executable = 'rspec'
  else
    let l:executable = &ft
  end

  if exists('a:1')
    call extend(l:cmd, [l:executable, l:path . ':' . a:1])
  else
    call extend(l:cmd, [l:executable, l:path])
  end

  let l:cmd_string = join(l:cmd, ' ')
  echo l:cmd_string

  silent exe 'TubeClr'
  silent exe 'Tube ' . l:cmd_string
endfunction

nmap <Leader>t :call TubeThis(line('.'))<CR>
nmap <Leader>T :call TubeThis()<CR>
" }}}

" Operator Surround {{{
map <silent>sa <Plug>(operator-surround-append)
map <silent>sd <Plug>(operator-surround-delete)
map <silent>sr <Plug>(operator-surround-replace)

nmap <silent>sdd <Plug>(operator-surround-delete)<Plug>(textobj-multiblock-a)
nmap <silent>srr <Plug>(operator-surround-replace)<Plug>(textobj-multiblock-a)

nmap <silent>sdd <Plug>(operator-surround-delete)<Plug>(textobj-anyblock-a)
nmap <silent>srr <Plug>(operator-surround-replace)<Plug>(textobj-anyblock-a)

nmap <silent>sdb <Plug>(operator-surround-delete)<Plug>(textobj-between-a)
nmap <silent>srb <Plug>(operator-surround-replace)<Plug>(textobj-between-a)
" }}}
