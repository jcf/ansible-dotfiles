#
# Defines Ruby on Rails aliases.
#
# Authors:
#   James Conroy-Finn <james@logi.cl>
#

#
# Aliases (Compatible with Rails 2)
#

alias rc='_rails-command console'
alias rdc='_rails-command dbconsole'
alias rg='_rails-command generate'
alias rr='_rails-command runner'
alias rs='_rails-command server'
alias rx='_rails-command destroy'
alias rl='tail -f log/development.log'

alias rk='_rake-command'
alias rdm='rk db:migrate'
alias rdM='rk db:migrate db:test:clone_structure'
alias rdr='rk db:rollback'
alias rlc='rk log:clear'

# Migrate and clone structure in to test database
alias migrate='rk db:migrate db:test:clone_structure'

#
# Functions
#

function _rails-command {
  if [[ -e ".zeus.sock" ]]; then
    zeus "$@"
  elif [[ -e "bin/rails" ]]; then
    bin/rails "$@"
  elif [[ -e "script/server" ]]; then
    ruby script/"$@"
  else
    ruby script/rails "$@"
  fi
}

function _rake-command {
  if [[ -e ".zeus.sock" ]]; then
    zeus rake "$@"
  else
    rake "$@"
  fi
}
