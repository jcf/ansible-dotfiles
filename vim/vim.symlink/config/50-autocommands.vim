augroup SourceVimrc " {{{
  autocmd!
  " When vimrc is edited, reload it {{{
  autocmd! bufwritepost ~/.vimrc source ~/.vimrc
  " }}}
augroup END " }}}

" Open Quickfix list after any grep-like command {{{
autocmd! QuickFixCmdPost *grep* cwindow
" }}}

augroup Whitespace " {{{
  autocmd!
  " Remove trailing whitespace from selected filetypes {{{
  function! s:StripTrailingWhitespace()
    normal mZ
    %s/\s\+$//e
    normal 'Z
  endfunction

  au FileType html,css,sass,javascript,coffee,python,ruby,clojure,psql,vim au BufWritePre <buffer> :silent! call <SID>StripTrailingWhitespace()
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

augroup Clojure " {{{
  autocmd!
  " Highlight pairs of parens {{{
  function! Rainbows()
    silent exe 'RainbowParenthesesLoadRound'
    silent exe 'RainbowParenthesesLoadSquare'
    silent exe 'RainbowParenthesesLoadBraces'
  endfunction

  au VimEnter * RainbowParenthesesActivate
  au FileType clojure call Rainbows()
  " }}}
augroup END " }}}

augroup JumpToLastPosition " {{{
  autocmd!
  " Jump to last position when returning to a buffer {{{
  function! PositionCursorFromViminfo()
    if !(bufname("%") =~ '\(COMMIT_EDITMSG\)') && line("'\"") > 1 && line("'\"") <= line("$")
      exe "normal! g`\""
    endif
  endfunction

  au BufReadPost * call PositionCursorFromViminfo()
  " }}}
augroup END " }}}

augroup Git " {{{
  " Setup gitcommit buffers with a text width and spell checking {{{
  autocmd! FileType gitcommit set spell
  " }}}
augroup END " }}}

augroup Haskell " {{{
  " :h ghc-compiler
  autocmd! BufEnter *.hs compiler ghc
augroup END " }}}

augroup Go " {{{
  " Automatically format our Go code when we write the current buffer to disk
  autocmd! FileType go autocmd BufWritePre <buffer> Fmt
augroup END " }}}

augroup CommentFormatOptions " {{{
  function! s:prevent_inserting_comments()
    setl formatoptions-=ro | setl formatoptions+=mM
  endfunction

  autocmd! FileType,Syntax * call s:prevent_inserting_comments()
augroup END " }}}
