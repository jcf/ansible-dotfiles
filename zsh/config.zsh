# vim: set syn=zsh

fpath=($DOT_FILES/zsh/functions $fpath)
autoload zmv
autoload -U $DOT_FILES/zsh/functions/*(:t)

# 256 colours
autoload spectrum && spectrum

setopt ALL_EXPORT

# Colour stuff
autoload colors zsh/terminfo

PAGER="less"
NODE_PATH="/usr/local/lib/node:$NODE_PATH"

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

ACK_COLOR_FILENAME="bold white on_black"
ACK_COLOR_MATCH="magenta on_black"

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
setopt extended_history
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

# by default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# we take out the slash, period, angle brackets and dash here.
export WORDCHARS='*?_[]~=&;!#$%^(){}'
