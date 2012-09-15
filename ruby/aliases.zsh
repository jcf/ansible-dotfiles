# bundler
alias be='bundle exec'
alias bc='bundle console'
alias bch='bundle check'
alias bi='bundle install'
alias bis='bundle install --standalone --binstubs'
alias bil='bundle install --local'
alias bu='bundle update'

# Start Guard using bundle exec
alias beg='bundle exec guard'

# Migrate and clone structure in to test database
alias migrate='rake db:migrate db:test:clone_structure'

# Tail the test log, ignoring TRUNCATE output from database_cleaner
alias rlog='tail -f log/test.log | grep -v TRUNCATE'

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
