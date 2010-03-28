" Display
syntax on
colorscheme mustang
set background=dark
set ruler
set ch=2 
set showcmd
set laststatus=2
set cursorline 
set hidden
set nonu

" Navigation
set showmatch
set matchtime=2
set incsearch
set ignorecase
set smartcase
set nohls
 
" Formatting
set autoindent
set preserveindent
set smartindent
set smarttab
set expandtab
set textwidth=78
set tabstop=2
set softtabstop=2
set shiftwidth=2
set formatoptions=tcroqn2 
set lbr
 
" Behavior
set noacd
set foldmethod=manual
set nofoldenable
set shellcmdflag=-c
set shell=zsh\ -l
set modeline
set modelines=5
set splitright
set scrolloff=3
set nomore
set wildmenu
set wildmode=list:longest
set backspace=indent,eol,start
set tabpagemax=100
set switchbuf=usetab
set history=5000
set shortmess=atI
set visualbell
set sessionoptions=blank,buffers,curdir,folds,help,resize,tabpages,winsize

set listchars=trail:.,tab:>-,eol:$
set nolist

filetype plugin on
filetype indent on

" Jump to last known location in file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal g'\"" | endif
endif
 
" Filetype based indent rules
if has("autocmd")
  filetype indent plugin on
endif
 
if has("gui_macvim")
    set transparency=10
    set guifont=Menlo:h11
    set lines=80 columns=140
    set guioptions=egmrt
    set formatoptions-=t
    set formatoptions-=c
    nmap <C-up> <C-y>
    imap <C-up> <C-o><C-y>
    nmap <C-down> <C-e>
    imap <C-down> <C-o><C-e>
endif
 
autocmd FileType ruby setlocal tabstop=2 softtabstop=2 shiftwidth=2 foldmethod=syntax
autocmd FileType mkd setlocal ai comments=n:>
autocmd BufRead *.rst setlocal filetype=rest
autocmd FileType rest setlocal ai comments=n:>
autocmd FileType yaml setlocal tabstop=2 shiftwidth=2 softtabstop=2
autocmd BufRead /etc/apache2/*,/etc/httpd/* setlocal filetype=apache
 
let mapleader = "," 

map <up> gk
inoremap <up> <C-R>=pumvisible() ? "\<lt>up>" : "\<lt>C-o>gk"<Enter>
map <down> gj
inoremap <down> <C-R>=pumvisible() ? "\<lt>down>" : "\<lt>C-o>gj"<Enter>
 
noremap <leader>i :set list!<CR>

nmap <C-N> :noh<CR>
nmap <S-Enter> O<ESC>
nmap <Enter> o<Esc>
map <leader>f :FuzzyFinderTextMate<CR>
map <leader>b :FuzzyFinderBuffer<CR>
imap jj <Esc>
imap uu _
imap hh =>
imap aa @

" bind command-] to shift right
nmap <D-]> >>
vmap <D-]> >>
imap <D-]> <C-O>>>

" bind command-[ to shift left
nmap <D-[> <<
vmap <D-[> <<
imap <D-[> <C-O><<

" open tabs with command-<tab number>
map <D-1> :tabn 1<CR>
map <D-2> :tabn 2<CR>
map <D-3> :tabn 3<CR>
map <D-4> :tabn 4<CR>
map <D-5> :tabn 5<CR>
map <D-6> :tabn 6<CR>
map <D-7> :tabn 7<CR>
map <D-8> :tabn 8<CR>
map <D-9> :tabn 9<CR>

noremap <leader>n :NERDTreeToggle<CR>

let g:netrw_liststyle = 3
let g:netrw_list_hide = '.*\.py[co]$,\.git$,\.swp$'

" User instead of Netrw when doing an edit /foobar
let NERDTreeHijackNetrw=1
" Single click for everything
let NERDTreeMouseMode=1

source ~/.vim/snippets/support_functions.vim
autocmd vimenter * call s:SetupSnippets()
function! s:SetupSnippets()

    "if we're in a rails env then read in the rails snippets
    if filereadable("./config/environment.rb")
        call ExtractSnips("~/.vim/snippets/ruby-rails", "ruby")
        call ExtractSnips("~/.vim/snippets/eruby-rails", "eruby")
    endif

    call ExtractSnips("~/.vim/snippets/html", "eruby")
    call ExtractSnips("~/.vim/snippets/html", "xhtml")
    call ExtractSnips("~/.vim/snippets/html", "php")
endfunction

" let g:speckyBannerKey = "<C-S>b"
let g:speckyQuoteSwitcherKey = "<C-S>'"
let g:speckyRunRdocKey = "<C-S>r"
let g:speckySpecSwitcherKey = "<leader>x"
let g:speckyRunSpecKey = "<C-S>s"
let g:speckyRunSpecCmd = "spec -fs -r loadpath.rb"
let g:speckyRunRdocCmd = "fri -L -f plain"
let g:speckyWindowType = 2

