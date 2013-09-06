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

" Airline {{{
  let g:airline_powerline_fonts = 1
  let g:airline_theme = 'solarized'

  set fillchars+=stl:\ ,stlnc:\  " Space
" }}}

" colorv {{{
  let g:colorv_no_global_map = 1  " Skip all colorv bindings
" }}}

" neocomplete {{{
  " Disable AutoComplPop
  let g:acp_enableAtStartup = 0

  " Use neocomplete
  let g:neocomplete#enable_at_startup = 1

  " Use smartcase
  let g:neocomplete#enable_smart_case = 1

  " Set minimum syntax keyword length
  let g:neocomplete#sources#syntax#min_keyword_length = 3
  let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

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

" NERDTree {{{
  let NERDTreeIgnore=['\.rbc$', '\.zsh\.zwc$', '\~$']
  " map <Leader>n :NERDTreeToggle<CR>
  " map <Leader>f :NERDTreeFind<CR>
" }}}

" Haskell {{{
  " Use default browser to browse online resources
  let g:haddock_browser = '/usr/bin/open'
" }}}

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
  if has('gui_running')
    " Use fancy symbols
    let g:Powerline_symbols = 'fancy'
  endif

  " Go Solarized
  let g:Powerline_theme = 'solarized256'
  let g:Powerline_colorscheme = 'solarized256'
" }}}

" Tube.vim {{{
  let g:tube_terminal = 'iterm' " Or Terminal if you prefer

  function! TubeThis(...) abort
    let l:cmd = []
    let l:path = expand('%')

    if filewritable('.zeus.sock')
      call add(l:cmd, 'zeus')
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

" Turbux {{{
  let g:no_turbux_mappings = 1
  let g:turbux_command_rspec='bundle exec rspec --tty'
  let g:turbux_command_cucumber='bundle exec cucumber'
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

" Highlight indentation level {{{
  let g:indentLine_color_gui = '#073642'
" }}}

" Ultisnips {{{
  let g:UltiSnipsSnippetDirectories  = ['snippets']
" }}}
