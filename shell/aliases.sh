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

# hub (http://github.com/defunkt/hub)
# brew install hub
[ `which hub` ] && alias git='nocorrect hub'

alias brew-update='brew install $(brew outdated | cut -d " " -f1 | tr "\n" " ")'

# Rails
function rails_command {
  local cmd=$1
  shift
  if [ -e script/rails ]; then
    rails $cmd "$@"
  elif [ -e script/$cmd ]; then
    script/$cmd "$@"
  else
    echo "This isn't a Rails app"
  fi
}
function ss { rails_command "server" "thin" "$@" }
function sc { rails_command "console" "$@" }
function sg { rails_command "generate" "$@" }
function sdb { rails_command "dbconsole" "$@" }

alias migrate='rake db:migrate db:test:clone_structure'

# bundler
alias be='bundle exec'
alias bc='bundle console'

# commands starting with % for pasting from web
alias %=' '

# What is my IP?
alias ip='curl http://www.whatismyip.com/automation/n09230945.asp'

alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"

# Node.js
alias node-repl='rlwrap node-repl'

# Readers and Pagers
alias t='tail -f'
