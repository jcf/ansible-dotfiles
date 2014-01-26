" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Ruby {{{
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-bundler'

" Insert 'end' automatically
NeoBundle 'tpope/vim-endwise'
" }}}

NeoBundle 'tpope/vim-rails'
NeoBundle 'hallison/vim-ruby-sinatra'

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
NeoBundle 'applescript.vim'
NeoBundle 'tpope/vim-cucumber'
NeoBundle 'ajf/puppet-vim'
NeoBundle 'jcf/cocoa.vim'
NeoBundle 'jcf/vim-latex'
NeoBundle 'leshill/vim-json'
NeoBundle 'jimenezrick/vimerl'
NeoBundle 'duwanis/tomdoc.vim'
NeoBundle 'jc00ke/vim-tomdoc'
" }}}

" Git {{{
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-git'

" Get the history behind some part of a file from Git
NeoBundle 'gregsexton/gitv', {
      \ 'depends': ['tpope/vim-fugitive'],
      \ 'autoload': {'commands': 'Gitv'}}
" }}}

" Better Haskell support {{{
NeoBundle 'lukerandall/haskellmode-vim'
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'dag/vim2hs'
NeoBundle 'ujihisa/neco-ghc'
NeoBundle 'pbrisbin/html-template-syntax'
" }}}

" Better Clojure support {{{
NeoBundle 'guns/vim-clojure-static'
NeoBundle 'tpope/vim-fireplace'
NeoBundle 'tpope/vim-classpath'
NeoBundle 'guns/vim-sexp'
NeoBundle 'tpope/vim-sexp-mappings-for-regular-people'
NeoBundle 'kien/rainbow_parentheses.vim'
NeoBundle 'jgdavey/vim-hearth' " Run tests from inside Vim
" }}}

" Colour schemes {{{
NeoBundle 'Lokaltog/vim-distinguished'
NeoBundle 'Wombat'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'sjl/badwolf'
NeoBundle 'tomasr/molokai'
NeoBundle 'tpope/vim-vividchalk'
NeoBundle 'w0ng/vim-hybrid'
NeoBundle 'wombat256.vim'
NeoBundle 'zeis/vim-kolor'
" }}}

" Search and display information from arbitrary sources like files, buffers,
" recently used files or registers
NeoBundle 'Shougo/unite.vim'

" Fancy file browser
NeoBundle 'Shougo/vimfiler.vim'
" Unite plugins {{{
NeoBundle 'tacroe/unite-mark'
NeoBundle 'sgur/unite-git_grep'
NeoBundle 'ujihisa/unite-locate'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'h1mesuke/unite-outline'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'tsukkee/unite-help'
" }}}

"Automatic Tag generation
NeoBundle 'xolox/vim-easytags', {'depends': ['xolox/vim-misc']}

" Use your Rbenv rubies in Vim
NeoBundle 'tpope/vim-rbenv'

" Make sessions like there's no tomorrow
NeoBundle 'tpope/vim-obsession'

" Merge tool inspired by Adobe Lightroom
NeoBundle 'killphi/splice.vim', 'fix_spliceinit_crash'

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

" Tag browser
NeoBundle 'majutsushi/tagbar'

" Orgmode
NeoBundle 'jceb/vim-orgmode'

" Preview substitution
NeoBundle 'osyo-manga/vim-over'

" Enhanced characters information, including Unicode character names, and
" emoji support.
NeoBundle 'tpope/vim-characterize'
" Improved % matching
NeoBundle 'matchit.zip'

" Lightweight status line replacement
NeoBundle 'bling/vim-airline'

" Improved and highly customisable completion
NeoBundle 'Shougo/neocomplete.vim'

" Add English word completion
NeoBundle 'ujihisa/neco-look'

" At match #N out of M matches
NeoBundle 'IndexedSearch'

" Like make but async, with support for tmux and more
NeoBundle 'tpope/vim-dispatch'

" Switch panes with C-hjkl
NeoBundle 'christoomey/vim-tmux-navigator'

" Control tmux from inside Vim!
NeoBundle 'mhinz/vim-tmuxify'

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
NeoBundle 'sjl/gundo.vim', {
      \ 'commands' : 'GundoToggle'
      \ }

" Replace quickfix with something more powerful
NeoBundle 'thinca/vim-qfreplace', {
      \ 'filetypes' : ['unite', 'quickfix'],
      \ }

" Move around quickly and effectively
NeoBundle 'Lokaltog/vim-easymotion'

" Abbreviation, substitution, coercion
NeoBundle 'tpope/vim-abolish'

" Syntax check files using external executables
NeoBundle 'scrooloose/syntastic'

" Comment with ease
NeoBundle 'tyru/caw.vim'

" Add support for Ag (the_silver_surfer)
NeoBundle 'epmatsw/ag.vim'

" Adds functions to remove/rename files etc.
NeoBundle 'tpope/vim-eunuch'

" Extends . to support repeating all kinds of operations
NeoBundle 'tpope/vim-repeat'

" Use CTRL-A/X to increment dates, times, and more
NeoBundle 'tpope/vim-speeddating'

" Surround with visual selections
NeoBundle 'kana/vim-operator-user', {
      \   'functions' : 'operator#user#define',
      \ }

NeoBundle 'rhysd/vim-operator-surround', {
      \   'mappings' : '<Plug>(operator-surround',
      \ }

NeoBundle 'kana/vim-operator-replace', {
      \ 'depends' : 'vim-operator-user',
      \ 'autoload' : {
      \   'mappings' : [
      \     ['nx', '<Plug>(operator-replace)']]
      \ }}

NeoBundle 'kana/vim-textobj-user'
NeoBundle 'thinca/vim-textobj-between'
NeoBundle 'rhysd/vim-textobj-anyblock'
NeoBundle 'osyo-manga/vim-textobj-multiblock'

" Improved visual-block mode
NeoBundle 'kana/vim-niceblock'

" Because Unite doesn't do fuzzy search well
NeoBundle 'kien/ctrlp.vim'

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

" Version control changes in signs
NeoBundle 'mhinz/vim-signify'

" Follow coding guidelines implemented with http://editorconfig.org/
NeoBundle 'editorconfig/editorconfig-vim'
