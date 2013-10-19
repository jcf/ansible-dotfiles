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

" Syntastic {{{
  " Mark syntax errors with :signs
  let g:syntastic_enable_signs = 1
  let g:syntastic_quiet_warnings = 1
  let g:syntastic_auto_loc_list = 1
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

" Highlight indentation level {{{
  let g:indentLine_color_gui = '#073642'
" }}}

" Ultisnips {{{
  let g:UltiSnipsSnippetDirectories  = ['snippets']
" }}}

" Paredit {{{
  " Disable paredit by default
  let g:paredit_mode = 0
" }}}
