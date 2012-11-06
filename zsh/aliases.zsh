alias -g A='|ack'
alias -g L='|less'
alias -g V='|vim -'
alias -g M='|mvim -'
alias -g G='|grep'
alias -g T='|tail'
alias -g H='|head'
alias -g N='&>/dev/null&'
alias -g W='|wc'

# MacVim will edit all!
alias e="$EDITOR"

# dotfiles
alias reload="source ~/.zshrc"
alias edot="$EDITOR -c \"cd $DOT\" $DOT"

# Lists the size of all the folders
alias ducks='du -cks * | sort -rn | head -11'

# Edit ssh config
alias essh="e ~/.ssh/config"

# List TCP ports
alias ports='sudo lsof -iTCP -sTCP:LISTEN -P'

# Host aware ls color option
case $(uname) in
  'Darwin')
    alias ls='ls -GF'
    ;;
  *)
    alias ls='ls --color=auto -F'
    ;;
esac

alias grep='grep --color'

# ls
alias l="ls -Alh"
alias ll="l"
alias l.='ls -ldG .*'
alias le='ls -aelG'
alias lsd="ls -al | awk '/^d/{print}'"

# commands starting with % for pasting from web
alias %=' '

# What is my IP?
alias ip='curl http://www.whatismyip.com/automation/n09230945.asp'

alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"

# Readers and Pagers
alias t='tail -f'

alias v='vagrant'

# Find SSH services via Bonjour
alias sshjour='dns-sd -B _ssh._tcp .'
