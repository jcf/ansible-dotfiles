augroup SourceVimrc " {{{
  autocmd!
  " When vimrc is edited, reload it {{{
    autocmd! bufwritepost ~/.vimrc source ~/.vimrc
  " }}}
augroup END " }}}

augroup Whitespace " {{{
  autocmd!
  " Remove trailing whitespace from selected filetypes {{{
    function! s:StripTrailingWhitespace()
      normal mZ
      %s/\s\+$//e
      normal `Z
    endfunction

    au FileType html,css,sass,javascript,php,python,ruby,psql,vim au BufWritePre <buffer> :silent! call <SID>StripTrailingWhitespace()
  " }}}
augroup END " }}}

augroup Applescript " {{{
  autocmd!
  " Set applescript filetype {{{
    au BufNewFile,BufRead *.applescript setf applescript
  " }}}
augroup END " }}}

augroup Markdown " {{{
  autocmd!
  " Setup wrapping for Markdown files
    function! s:setupWrapping()
      set wrap
      set wm=2
      set textwidth=72
    endfunction

    function! s:setupMarkup()
      call s:setupWrapping()
      map <buffer> <Leader>p :Mm <CR>
    endfunction

    " md, markdown, and mk are markdown and define buffer-local preview
    au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn} call s:setupMarkup()
  " }}}
augroup END " }}}

augroup JumpToLastPosition " {{{
  autocmd!
  " Jump to last position when returning to a buffer {{{
    function! SetCursorPosition()
      if &filetype !~ 'commit\c'
        if line("'\"") > 0 && line("'\"") <= line("$")
          exe "normal! g`\""
          normal! zz
        endif
      end
    endfunction

    au BufReadPost * call SetCursorPosition()
  " }}}
augroup END " }}}
