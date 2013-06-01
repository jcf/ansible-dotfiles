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

" Powerline {{{
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

  set fillchars+=stl:\ ,stlnc:\
" }}}

" Tube.vim {{{
  let g:tube_terminal = 'iterm' " Or Terminal if you prefer

  function! TubeThis(...) abort
    let l:cmd = []
    let l:path = expand('%')

    if filewritable('.zeus.sock')
      call add(l:cmd, 'zeus')
    " elseif filereadable('Gemfile')
    "   call add(l:cmd, 'bundle exec')
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

    silent exe 'TubeClear'
    silent exe 'Tube ' . l:cmd_string
  endfunction

  nmap <Leader>t :call TubeThis(line('.'))<CR>
  nmap <Leader>T :call TubeThis()<CR>
" }}}

" Run Hammer to preview this buffer {{{
  nmap <Leader>p :Hammer<CR>
" }}}

" colorv {{{
  let g:colorv_no_global_map = 1  " Skip all colorv bindings
" }}}

" neocompcache {{{
  let g:neocomplcache_enable_at_startup = 1
" }}}

" NERDTree {{{
  let NERDTreeIgnore=['\.rbc$', '\.zsh\.zwc$', '\~$']
  " map <Leader>n :NERDTreeToggle<CR>
  " map <Leader>f :NERDTreeFind<CR>
" }}}

" Haskell {{{
  " Use default browser to browse online resources
  let g:haddock_browser = '/usr/bin/open'
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

" Powerline {{{
  " Use fancy symbols
  let g:Powerline_symbols = 'fancy'

  " Go Solarized
  let g:Powerline_theme = 'solarized256'
  let g:Powerline_colorscheme = 'solarized256'
" }}}

" Vim Ruby Conque {{{
  " Cmd-Shift-R for RSpec
  nmap <silent> <D-R> :call RunRspecCurrentFileConque()<CR>

  " Cmd-Shift-L for RSpec Current Line
  nmap <silent> <D-L> :call RunRspecCurrentLineConque()<CR>

  " ,Cmd-R for Last conque command
  nmap <silent> <Leader><D-R> :call RunLastConqueCommand()<CR>

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

" Switch.vim {{{
  nnoremap <Leader>s :Switch<CR>
" }}}

" Tabular.vim {{{
  vnoremap <Leader>t: :Tabularize /:\zs<CR>
  vnoremap <Leader>t= :Tabularize /=<CR>
  vnoremap <Leader>t> :Tabularize /=><CR>
" }}}

" Highlight indentation level {{{
  let g:indentLine_color_gui = '#073642'
" }}}

" VimWiki {{{
  let g:vimwiki_list = [{'path': '~/Dropbox/Wiki',
        \ 'syntax': 'markdown',
        \ 'ext': '.md'}]
" }}}
