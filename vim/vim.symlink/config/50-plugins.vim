" Open Vundle config quickly {{{
  nnoremap <Leader>vu :e ~/.dotfiles/vim/vim.symlink/config/00-vundle.vim<CR>
" }}}

" Run Hammer to preview this buffer {{{
  nmap <Leader>p :Hammer<CR>
" }}}

" Toggle tagbar {{{
  nmap <Leader>b :TagbarToggle<CR>
  let g:tagbar_ctags_bin = '/usr/local/bin/ctags'

  let g:tagbar_type_javascript = {
        \ 'ctagsbin' : '/usr/local/bin/jsctags'
        \ }

  let g:tagbar_type_coffee = {
        \ 'ctagstype' : 'coffee',
        \ 'kinds' : [
        \   'c:classes',
        \   'f:functions',
        \   'v:variables'
        \ ],
        \ }
" }}}

" neocompcache {{{
  let g:neocomplcache_enable_at_startup = 1
" }}}

" NERDTree {{{
  let NERDTreeIgnore=['\.rbc$', '\.zsh\.zwc$', '\~$']
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

  " Go Solarized
  let g:Powerline_theme = 'skwp'
  let g:Powerline_colorscheme = 'skwp'
" }}}

" VimGrep {{{
  " Use grep (follows grepprg, so probably Ack)
  let EasyGrepCommand = 1
" }}}

" Vim Ruby Conque {{{
  " Cmd-Shift-R for RSpec
  nmap <silent> <D-R> :call RunRspecCurrentFileConque()<CR>

  " Cmd-Shift-L for RSpec Current Line
  nmap <silent> <D-L> :call RunRspecCurrentLineConque()<CR>

  " ,Cmd-R for Last conque command
  nmap <silent> ,<D-R> :call RunLastConqueCommand()<CR>

  " Use zeus to run specs & cukes
  let g:ruby_conque_rspec_command='zeus rspec'
  let g:turbux_command_rspec='zeus rspec'
  let g:turbux_command_cucumber='zeus cucumber'
" }}}

" Splice {{{
  let g:splice_initial_mode = 'grid'
  let g:splice_initial_wrap = 'nowrap'
  let g:splice_initial_layout_grid = 1
  let g:splice_initial_layout_compare = 0
" }}}

" Golden Ratio {{{
  let g:golden_ratio_autocommand = 0
" }}}
