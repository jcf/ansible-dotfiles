#
# Defines Ruby on Rails aliases.
#
# Authors:
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

alias r='rails'
alias rc='_rails-command console'
alias rdc='_rails-command dbconsole'
alias rdm='rake db:migrate'
alias rdM='rake db:migrate db:test:clone'
alias rdr='rake db:rollback'
alias rg='_rails-command generate'
alias rl='tail -f log/development.log'
alias rlc='rake log:clear'
alias rp='_rails-command plugin'
alias rr='_rails-command runner'
alias rs='_rails-command server'
alias rsd='_rails-command server --debugger'
alias rx='_rails-command destroy'

# Migrate and clone structure in to test database
alias migrate='rake db:migrate db:test:clone_structure'

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
  if [[ -e "script/server" ]]; then
    ruby script/"$@"
  else
    ruby script/rails "$@"
  fi
}
