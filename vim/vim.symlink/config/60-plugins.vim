" Airline {{{
let g:airline_powerline_fonts = 1
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

" Avoid errors related to Clojure-complete clashing with NeoComplete.
let g:neocomplete#force_overwrite_completefunc = 1
" }}}

" Haskell {{{
" Use default browser to browse online resources
let g:haddock_browser = '/usr/bin/open'
" }}}

" Syntastic {{{
" Mark syntax errors with :signs
let g:syntastic_enable_signs = 1
let g:syntastic_quiet_messages = {'level': 'warnings'}

" Don't open loc list automatically - it's really disruptive
let g:syntastic_auto_loc_list = 0
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
let g:indentLine_enabled = 0
" }}}

" Ultisnips {{{
let g:UltiSnipsSnippetDirectories  = ['snippets']
" }}}

" Signify {{{
let g:signify_disable_by_default = 1
" }}}

" Rainbow Parentheses {{{
" Remove black. It's bloody invisible.
let g:rbpt_colorpairs = [
      \ ['brown',       'RoyalBlue3'],
      \ ['Darkblue',    'SeaGreen3'],
      \ ['darkgray',    'DarkOrchid3'],
      \ ['darkgreen',   'firebrick3'],
      \ ['darkcyan',    'RoyalBlue3'],
      \ ['darkred',     'SeaGreen3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['brown',       'firebrick3'],
      \ ['gray',        'RoyalBlue3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['Darkblue',    'firebrick3'],
      \ ['darkgreen',   'RoyalBlue3'],
      \ ['darkcyan',    'SeaGreen3'],
      \ ['darkred',     'DarkOrchid3'],
      \ ['red',         'firebrick3'],
      \ ]

" One less colour without black
let g:rbpt_max = 15
" }}}

" Vimfiler {{{
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_data_directory = expand('~/.vim/tmp/vimfiler/')
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_execute_file_list = { "_": "vim" }
" }}}

" unite-ref {{{
let g:ref_use_vimproc = 1
let g:ref_open = 'vsplit'
let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/')
nno <leader>K :<C-u>Unite ref/ri -buffer-name=erlang_docs -start-insert
      \ -vertical -default-action=split<CR>
" }}}

" netrw {{{
let g:netrw_http_cmd='curl -0 -k -L -vv'
let g:netrw_http_xcmd='-o'
" }}}
