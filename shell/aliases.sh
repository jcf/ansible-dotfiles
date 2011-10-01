# MacVim will edit all!
alias e="$EDITOR"

# dotfiles
alias reload="source ~/.zshrc"
alias cdot="cd ~/.dotfiles"
alias edot="mvim -c \"cd ~/.dotfiles\" ~/.dotfiles"
#
# Lists the size of all the folders
alias ducks='du -cks * | sort -rn |head -11'

# Edit ssh config
alias essh="e ~/.ssh/config"

# cd
alias ..='cd ..;'
alias ...='.. ..'

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

alias migrate='rake db:migrate db:test:clone_structure'

# bundler
alias be='bundle exec'
alias bc='bundle console'
alias bch='bundle check'
alias bi='bundle install'
alias bil='bundle install --local'
alias bu='bundle update'

# Start Guard using bundle exec
alias beg='bundle exec guard'

# commands starting with % for pasting from web
alias %=' '

# What is my IP?
alias ip='curl http://www.whatismyip.com/automation/n09230945.asp'

alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"

# Node.js
alias node-repl='rlwrap node-repl'

# Readers and Pagers
alias t='tail -f'
