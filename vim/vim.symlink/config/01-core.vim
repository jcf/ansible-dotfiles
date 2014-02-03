" Basic configuration {{{
filetype off
syntax on
filetype plugin indent on

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" store lots of :cmdline history
set history=1000

set noshowcmd   " Don't show incomplete commands
set noshowmode  " Hide current mode

set incsearch   " find the next match as we type the search
set hlsearch    " Highlight search results

set whichwrap+=h,l,<,>,[,],b,s,~
set linebreak
set showbreak=>\
set nowrap

set number      " line numbers
set laststatus=2

" No more tubular bells
set noerrorbells
set visualbell

" Quieten some of those interruptive prompts
set shortmess=aoOtTI

" Stop highlighting ridiculously long lines
set synmaxcol=400

" Indent settings
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent

" Fold settings
set foldenable
set foldmethod=marker
set foldlevel=3
set foldnestmax=10

" Default keyword/help program to Vim's internal help instead of man
set keywordprg=:help

" Tab completion
set wildmenu
set wildmode=list:longest,list:full
set wildignore=*.o,*.obj,*~,.git,*.rbc

" Display extended information when suggesting completions
set showfulltag

" Ignore case during completion
set infercase

" Clever, lazy case sensitive searches
set ignorecase
set smartcase

" Display tabs and trailing spaces
set list
set listchars=tab:▷\ ,trail:-,extends:»,precedes:«,nbsp:⋅

" Vertical/horizontal scroll off settings
set scrolloff=3
set sidescrolloff=7
set sidescroll=1

" Some stuff to get the mouse going in term
set mouse=a
set ttymouse=xterm2

set shell=zsh\ -l

" Use tagfile to complete commands
set wildoptions=tagfile

" One line for commands. Although two will often supress some naggy
" commands, it's too much within tmux.
set cmdheight=1

" Hide buffers when not displayed
set hidden

" Automatically read and write buffers
set autoread
set autowrite

" No backup files!
set nobackup
set nowritebackup
set noswapfile

set pastetoggle=<F2>

set spelllang=en spellfile=~/.vim/spell/en.utf-8.add

" Don't insert a comment when pressing o/O
set formatoptions-=o

set undofile
set undodir=~/.vim/tmp/undo
set undolevels=1000
set undoreload=10000

set backupdir=~/.vim/tmp/backup
set directory=~/.vim/tmp/swap

if executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
endif

if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
endif
" }}}

function! EnsureDirectoryExists(path) " {{{
if !isdirectory(expand(a:path))
  call mkdir(expand(a:path))
endif
endfunction " }}}

call EnsureDirectoryExists(&undodir)
call EnsureDirectoryExists(&backupdir)
call EnsureDirectoryExists(&directory)
