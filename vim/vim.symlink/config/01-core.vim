
" Basic configuration {{{
  filetype off
  syntax on
  filetype plugin indent on

  " allow backspacing over everything in insert mode
  set backspace=indent,eol,start

  " store lots of :cmdline history
  set history=1000

  set showcmd     " show incomplete cmds down the bottom
  set noshowmode  " hide current mode

  set incsearch   " find the next match as we type the search
  set hlsearch    " hilight searches by default

  set nowrap      " dont wrap lines
  set linebreak   " wrap lines at convenient points
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
  set foldmethod=indent   " Fold based on indent
  set foldlevel=1         " Fold one level of indentation in
  set foldnestmax=10      " Deepest fold is 10 levels
  set nofoldenable        " Don't fold by default

  " Tab completion
  set wildmenu
  set wildmode=list:longest,list:full
  set wildignore=*.o,*.obj,*~,.git,*.rbc

  " Display tabs and trailing spaces
  set list
  set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

  " Vertical/horizontal scroll off settings
  set scrolloff=3
  set sidescrolloff=7
  set sidescroll=1

  " Some stuff to get the mouse going in term
  set mouse=a
  set ttymouse=xterm2

  set shell=zsh\ -l

  " Two lines for commands. This tends to shut up some naggy commands.
  set cmdheight=2

  " Tell the term has 256 colors
  set t_Co=256

  " Hide buffers when not displayed
  set hidden

  " Automatically read and write buffers
  set autoread
  set autowrite

  " Clever, lazy case sensitive searches
  set ignorecase
  set smartcase

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
" }}}

function! EnsureDirectoryExists(path) " {{{
  if !isdirectory(expand(a:path))
    call mkdir(expand(a:path))
  endif
endfunction " }}}

call EnsureDirectoryExists(&undodir)
call EnsureDirectoryExists(&backupdir)
call EnsureDirectoryExists(&directory)
