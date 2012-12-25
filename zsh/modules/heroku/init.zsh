#
# Adds some helpful Heroku aliases.
#
# Authors: James Conroy-Finn <james@logi.cl>
#

if (( ! $+commands[heroku] )); then
  return
fi

alias heroku='nocorrect heroku'

alias h='heroku'
alias hc='heroku console'
alias hr='heroku run'
alias hv='heroku config'
