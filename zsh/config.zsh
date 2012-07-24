# vim: set syn=zsh

setopt ALL_EXPORT

# Autoload Zsh functions.
autoload -Uz age
autoload -Uz cdr
autoload -Uz zargs
autoload -Uz zcalc
autoload -Uz zmv

# Colour stuff
autoload colors zsh/terminfo

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

unsetopt ALL_EXPORT

# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

setopt auto_cd          # lazy directory navigation
setopt nobeep
setopt autopushd
setopt pushdminus
setopt pushdsilent
setopt pushdtohome
setopt globdots
setopt no_bg_nice       # don't nice background tasks
setopt no_hup
setopt no_list_beep
setopt local_options    # allow functions to have local options
setopt local_traps      # allow functions to have local traps
setopt prompt_subst
setopt correct
setopt complete_in_word
setopt ignore_eof
setopt alwaystoend      # when complete from middle, move cursor

setopt append_history         # adds history
setopt extended_history       # add timestamps to history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups   # don't record dupes in history
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_verify
setopt inc_append_history     # adds history incrementally and share it across sessions
setopt share_history          # share history between sessions ???

zle -N newtab
bindkey -e # because vi mode sucks
bindkey '^[[5D' backward-word
bindkey '^[[5C' forward-word
bindkey ' ' magic-space

# Find partial history matches
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Copy previous word for moves and copies
bindkey "^[m" copy-prev-shell-word

# by default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# we take out the slash, period, angle brackets and dash here.
WORDCHARS='*?_[]~=&;!#$%^(){}'
# WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
