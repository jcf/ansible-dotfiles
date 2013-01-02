#
# Defines Ruby on Rails aliases.
#
# Authors:
#   James Conroy-Finn <james@logi.cl>
#   Robby Russell <robby@planetargon.com>
#   Jake Bell <jake.b.bell@gmail.com>
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Return if requirements are not found.
if (( ! $+commands[rails] )); then
  return 1
fi

#
# Aliases (Compatible with Rails 2)
#

alias r='_rails-command'
alias rc='r console'
alias rdc='r dbconsole'
alias rg='r generate'
alias rp='r plugin'
alias rr='r runner'
alias rs='r server'
alias rsd='r server --debugger'
alias rx='r destroy'
alias rl='tail -f log/development.log'

alias rdm='_rake-command db:migrate'
alias rdM='_rake-command db:migrate db:test:clone'
alias rdr='_rake-command db:rollback'
alias rlc='_rake-command log:clear'

# Migrate and clone structure in to test database
alias migrate='_rake-command db:migrate db:test:clone_structure'

# Zeus commands
alias z='zeus start'
alias zg='zeus generate'
alias zs='zeus server'
alias zc='zeus console'
alias zr='zeus rspec'
alias zk='zeus cucumber'
alias zrn='zeus runner'
alias zdb='zeus dbconsole'
alias zrake='zeus rake'

#
# Functions
#

function _rails-command {
  if [[ -e ".zeus.sock" ]]; then
    zeus "$@"
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
