" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Ruby {{{
  NeoBundle 'vim-ruby/vim-ruby'
  NeoBundle 'tpope/vim-rake'
  NeoBundle 'tpope/vim-bundler'

  " Insert 'end' automatically
  NeoBundle 'tpope/vim-endwise'
" }}}

" Web Development {{{
  NeoBundle 'tpope/vim-rails'
  NeoBundle 'hallison/vim-ruby-sinatra'

  " Haskell template support
  NeoBundle 'pbrisbin/html-template-syntax'

  " Open the system color picker
  NeoBundle 'PickAColor.vim'

  " Print colour representations in colour!
  NeoBundle 'Rykka/colorv.vim'

  " Ease manipulation of dynamic HTML templates
  NeoBundle 'tpope/vim-ragtag'

  " Convert markup to HTML
  NeoBundle 'matthias-guenther/hammer.vim'

  " Run commands in your Terminal
  NeoBundle 'gcmt/tube.vim'

  " Additional filetypes {{{
    NeoBundle 'jcf/go.vim'
    NeoBundle 'wavded/vim-stylus'
    NeoBundle 'slim-template/vim-slim'
    NeoBundle 'tpope/vim-haml'
    NeoBundle 'groenewege/vim-less'
    NeoBundle 'kchmck/vim-coffee-script'
    NeoBundle 'pangloss/vim-javascript'
    NeoBundle 'statianzo/vim-jade'
    NeoBundle 'timcharper/textile.vim'
    NeoBundle 'tpope/vim-liquid'
    NeoBundle 'tpope/vim-markdown'
    NeoBundle 'itspriddle/vim-jquery'
    NeoBundle 'nono/vim-handlebars'
    NeoBundle 'mutewinter/nginx.vim'
    NeoBundle 'hail2u/vim-css3-syntax'
  " }}}
" }}}

" Git {{{
  NeoBundle 'tpope/vim-fugitive'
  NeoBundle 'tpope/vim-git'

  " Get the history behind some part of a file from Git
  NeoBundle 'gregsexton/gitv'
" }}}

" Additional filetypes {{{
  NeoBundle 'applescript.vim'
  NeoBundle 'tpope/vim-cucumber'
  NeoBundle 'ajf/puppet-vim'
  NeoBundle 'jcf/cocoa.vim'
  NeoBundle 'jcf/vim-latex'
  NeoBundle 'leshill/vim-json'
  NeoBundle 'jimenezrick/vimerl'
  NeoBundle 'lukerandall/haskellmode-vim'

  NeoBundle 'guns/vim-clojure-static'
  NeoBundle 'tpope/vim-fireplace'
  NeoBundle 'tpope/vim-classpath'

  " Syntax add-on for TomDoc comments
  NeoBundle 'duwanis/tomdoc.vim'

  " Some helper functions to add TomDoc templates to your Ruby code
  NeoBundle 'jc00ke/vim-tomdoc'
" }}}

" Colour schemes {{{
  NeoBundle 'altercation/vim-colors-solarized'
  NeoBundle 'Lokaltog/vim-distinguished'
  NeoBundle 'tpope/vim-vividchalk'
" }}}

" Vim modes {{{
  " Use your Rbenv rubies in Vim
  NeoBundle 'tpope/vim-rbenv'

  " Make sessions like there's no tomorrow
  NeoBundle 'tpope/vim-obsession'

  " File browser
  NeoBundle 'scrooloose/nerdtree'

  " Merge tool inspired by Adobe Lightroom
  NeoBundle 'sjl/splice.vim'

  " Add a scratch buffer to keep hold of random snippets
  NeoBundle 'duff/vim-scratch'

  " Async external commands
  " Make sure to run `make -f make_mac.mak`
  NeoBundle 'Shougo/vimproc', {
        \ 'build' : {
        \     'windows' : 'make -f make_mingw32.mak',
        \     'cygwin' : 'make -f make_cygwin.mak',
        \     'mac' : 'make -f make_mac.mak',
        \     'unix' : 'make -f make_unix.mak',
        \    },
        \ }

  " Search and display information from arbitrary sources like files, buffers,
  " recently used files or registers
  NeoBundle 'Shougo/unite.vim'

  " Fancy file browser
  NeoBundle 'Shougo/vimfiler.vim'
" }}}

" Unite plugins {{{
  NeoBundle 'tacroe/unite-mark'
  NeoBundle 'sgur/unite-git_grep'
  NeoBundle 'ujihisa/unite-locate'
  NeoBundle 'tsukkee/unite-tag'
  NeoBundle 'tsukkee/unite-help'
" }}}

" Vim powerups {{{
  " Improved % matching
  NeoBundle 'matchit.zip'

  " Improved status line
  NeoBundle 'Lokaltog/powerline'

  " Improved and highly customisable completion
  NeoBundle 'Shougo/neocomplete.vim'

  " At match #N out of M matches
  NeoBundle 'IndexedSearch'

  " Like make but async, with support for tmux and more
  NeoBundle 'tpope/vim-dispatch'

  " Maps from files to tests, and works with dispatch!
  NeoBundle 'jgdavey/vim-turbux'

  " Speed up handling of large files by turning off some features
  NeoBundle 'LargeFile'

  " Golden-ratio splits
  NeoBundle 'roman/golden-ratio'

  " Identation-based text objects
  NeoBundle 'michaeljsmith/vim-indent-object'

  " Use space to repeat yourself
  NeoBundle 'spiiph/vim-space'

  " TextMate style snippets, powered by Python
  NeoBundle 'SirVer/ultisnips'

  " Enables opening files at a specific line (e.g. file.rb:11)
  NeoBundle 'bogado/file-line'

  " Visual undo
  NeoBundle 'sjl/gundo.vim'

  " Move around quickly and effectively
  NeoBundle 'Lokaltog/vim-easymotion'

  " Abbreviation, substitution, coercion
  NeoBundle 'tpope/vim-abolish'

  " Syntax check files using external executables
  NeoBundle 'scrooloose/syntastic'

  " Comment with ease
  NeoBundle 'tpope/vim-commentary'

  " Add support for Ag (the_silver_surfer)
  NeoBundle 'epmatsw/ag.vim'

  " Adds functions to remove/rename files etc.
  NeoBundle 'tpope/vim-eunuch'

  " Repeat the last Tim Popeism you performed
  NeoBundle 'tpope/vim-repeat'

  " Use CTRL-A/X to increment dates, times, and more
  NeoBundle 'tpope/vim-speeddating'

  " Surround with visual selections
  NeoBundle 'tpope/vim-surround'

  " Complementary pairs of mappings
  NeoBundle 'tpope/vim-unimpaired'

  " Align text based on a given pattern
  NeoBundle 'godlygeek/tabular'

  " Highlight levels of indentation
  NeoBundle 'Yggdroot/indentLine'

  " Zoom in on the current buffer hiding others
  NeoBundle 'ZoomWin'

  " Make it easier to work with files owned by root
  NeoBundle 'sudo.vim'

  " GPG help
  NeoBundle 'jamessan/vim-gnupg'

  " Vim Ref
  NeoBundle 'ref.vim'
  NeoBundle 'taka84u9/vim-ref-ri'
" }}}
