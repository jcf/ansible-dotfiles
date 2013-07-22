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

    au FileType html,css,sass,javascript,coffee,python,ruby,psql,vim au BufWritePre <buffer> :silent! call <SID>StripTrailingWhitespace()
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

augroup Session " {{{
  autocmd!
  " Load the Session.vim in current directory {{{
    function! LoadSession()
      let b:sessionfile = getcwd() . '/Session.vim'
      if (filereadable(b:sessionfile)) && &filetype != "gitcommit"
        exe 'source ' b:sessionfile
      endif
    endfunction

    au VimEnter * nested :call LoadSession()
  " }}}
augroup END" }}}

" TODO Stop inserting comments when pressing o/O!!!
"
" This doesn't work because Vim executes ftplugin files, which typically
" trample all over my preferences.
"
" I could add an after/<ftype>.vim file and set this for every filetype that
" messes with my settings but that would be ridiculous.
"
" If anyone has a solution to this problem please share it with me because
" http://vim.wikia.com/wiki/Disable_automatic_comment_insertion doesn't work.
"
" augroup StopInsertingComments " {{{
"   autocmd!
"   " Don't insert a comment when pressing o/O {{{
"     au FileType * au BufRead,BufNewFile <buffer> setlocal formatoptions-=c formatoptions-=r formatoptions-=o
"   " }}}
" augroup END " }}}
