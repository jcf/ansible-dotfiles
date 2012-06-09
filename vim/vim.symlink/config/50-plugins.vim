" Open Vundle config quickly {{{
  nnoremap <Leader>vu :e ~/.vim/config/00-vundle.vim<CR>
" }}}

" Run Hammer to preview this buffer {{{
  nmap <Leader>p :Hammer<CR>
" }}}

" Toggle tagbar {{{
  nmap <Leader>t :TagbarToggle<CR>
" }}}

" NERDTree {{{
  let NERDTreeIgnore=['\.rbc$', '\~$']
  map <Leader>n :NERDTreeToggle<CR>
" }}}

" Rake.vim {{{
  nmap <Leader>a :AV<CR>
  nmap <C-a> :A<CR>
" }}}

" Bufexplorer {{
  nnoremap <C-B> :BufExplorer<cr>
" }}

" PickHEX {{{
  imap <D-C> <c-o>:PickHEX<CR>
  nmap <D-C> :PickHEX<CR>
" }}}

" Syntastic {{{
  " Mark syntax errors with :signs
  let g:syntastic_enable_signs=1
  let g:syntastic_quiet_warnings=1
" }}}

" NERDCommenter {{{
  let NERDSpaceDelims=1
" }}}

" Powerline {{{
  " Use fancy symbols
  let g:Powerline_symbols = 'fancy'
" }}}

