augroup UniteAutoCmd
  autocmd!
augroup END

function! s:unite_tabs_and_windows()
  imap <buffer> <C-h> <Esc><C-h>
  imap <buffer> <C-j> <Esc><C-j>
  imap <buffer> <C-k> <Esc><C-k>
  imap <buffer> <C-l> <Esc><C-l>
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
nno <leader>u :<C-u>UniteResume<CR>

function! s:unite_settings()
  imap <buffer> jj <Plug>(unite_insert_leave)
  imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
  imap <buffer> <leader> <Esc><leader>

  " Fuzzy matching works really, really badly in any reasonably large project.
  " call unite#filters#matcher_default#use(['matcher_fuzzy'])

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

" Vimfiler {{{
nno <leader>n :<C-u>:VimFilerCurrentDir -buffer-name=explorer -toggle<CR>
nno <leader>N :<C-u>:VimFilerBufferDir -buffer-name=explorer -toggle<CR>

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
