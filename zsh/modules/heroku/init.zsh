#
# Adds some helpful Heroku aliases.
#
# Authors: James Conroy-Finn <james@logi.cl>
#

if (( ! $+commands[heroku] )); then
  return
fi

alias heroku='nocorrect heroku'

alias h='heroku-app'
alias hc='h run console'
alias hr='h run'
alias hv='h config'
alias hl='h logs'
