augroup UniteAutoCmd
  autocmd!
augroup END

function! s:unite_tabs_and_windows()
  nmap <buffer> <C-h> <C-w>h
  nmap <buffer> <C-j> <C-w>j
  nmap <buffer> <C-k> <C-w>k
  nmap <buffer> <C-l> <C-w>l
  imap <buffer> <C-h> <Esc><C-w>h
  imap <buffer> <C-j> <Esc><C-w>j
  imap <buffer> <C-k> <Esc><C-w>k
  imap <buffer> <C-l> <Esc><C-w>l
  nmap <buffer> H gT
  nmap <buffer> L gt
  nmap <buffer> <leader>x :bd!<CR>
endfunction

let g:unite_data_directory = '~/.vim/tmp/unite/'
let g:unite_source_process_enable_confirm = 1
let g:unite_source_history_yank_enable = 1
let g:unite_enable_split_vertically = 0
let g:unite_winheight = 10
let g:unite_source_file_mru_filename_format = ':~:.'

nno <leader>s :<C-u>Unite grep:. -default-action=above -winheight=30 -auto-preview<CR>
nno <leader>S :<C-u>execute 'Unite grep:.::' . expand("<cword>") . ' -default-action=above -winheight=30 -auto-preview'<CR>
nno <leader>b :<C-u>Unite buffer -buffer-name=buffers -start-insert<CR>
" nno <leader><leader> :<C-u>UniteWithCurrentDir buffer file -buffer-name=united -start-insert<CR>
nno <leader>ps :<C-u>:Unite process -buffer-name=processes -start-insert<CR>
nno <leader>u :<C-u>UniteResume<CR>
nno <C-p> :<C-u>:Unite history/yank -buffer-name=yanks<CR>
" nno // :<C-u>:Unite line -buffer-name=lines -start-insert -direction=botright -winheight=10<CR>

function! s:unite_settings()
  imap <buffer> jj <Plug>(unite_insert_leave)
  imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
  imap <buffer> <leader> <Esc><leader>

  call unite#filters#matcher_default#use(['matcher_fuzzy'])

  call s:unite_tabs_and_windows()
endfunction

autocmd UniteAutoCmd FileType unite call s:unite_settings()

" Open files quickly {{{
  map <leader>f :<C-u>execute 'Unite file_rec/async file/new ' .
        \'-buffer-name=files -start-insert -toggle'<CR>
" }}}

" Lookup the current word via ctags {{{
  map <leader>w :UniteWithCursorWord -immediately tag<CR>
" }}}

" Jump to a project {{{
  map <leader>P :<C-u>execute 'Unite directory:' . expand('~/Code') .
        \ ' directory_mru ' .
        \ '-buffer-name=cdable -profile-name=cdable ' .
        \ '-start-insert -toggle -default-action=cd'<CR>
" }}}

" Open Vim configs quickly {{{
  map <leader>vc :<C-u>execute 'Unite file_rec/async:' . expand('~/.vim/config') .
        \ ' -buffer-name=configs ' .
        \ '-start-insert -toggle -profile-name=files'<CR>
" }}}

" Open dotfiles quickly {{{
  map <leader>df :<C-u>execute 'Unite file_rec/async:' . expand('~/.dotfiles') .
        \ ' -buffer-name=files -start-insert -toggle'<CR>
" }}}

" VimFiler {{{
  let g:vimfiler_as_default_explorer = 1
  let g:vimfiler_data_directory = expand('~/.vim/tmp/vimfiler/')
  let g:vimfiler_safe_mode_by_default = 0
  let g:vimfiler_execute_file_list = { "_": "vim" }

  nno <leader>n :<C-u>:VimFilerCurrentDir -buffer-name=explorer -toggle<CR>
  nno <leader>N :<C-u>:VimFilerBufferDir -buffer-name=explorer -toggle<CR>
  " nno ` :<C-u>:VimFilerBufferDir -buffer-name=explorer -toggle<CR>

  function! s:vimfiler_settings()
    call s:unite_tabs_and_windows()
    nmap <buffer> - <Plug>(vimfiler_switch_to_parent_directory)
    nmap <buffer> % <Plug>(vimfiler_new_file)
    nmap <buffer> <Backspace> <C-^>
    nmap <buffer> <leader>x <Plug>(vimfiler_exit)
    nmap <buffer> <leader>X <Plug>(vimfiler_exit)
  endfunction

  autocmd UniteAutoCmd Filetype vimfiler call s:vimfiler_settings()
" }}}

" Ref {{{
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
