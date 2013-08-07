#
# Configures Ruby local gem installation, loads version managers, and defines
# aliases.
#
# Authors: Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Load RVM into the shell session.
if [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
  # Unset AUTO_NAME_DIRS since auto adding variable-stored paths to ~ list
  # conflicts with RVM.
  unsetopt AUTO_NAME_DIRS

  # Source RVM.
  source "$HOME/.rvm/scripts/rvm"

# Load manually installed rbenv into the shell session.
elif [[ -s "$HOME/.rbenv/bin/rbenv" ]]; then
  path=("$HOME/.rbenv/bin" $path)
  eval "$(rbenv init --no-rehash - zsh)"

# Load package manager installed rbenv into the shell session.
elif (( $+commands[rbenv] )); then
  eval "$(rbenv init --no-rehash - zsh)"

# Install local gems according to operating system conventions.
else
  if [[ "$OSTYPE" == darwin* ]]; then
    export GEM_HOME="$HOME/Library/Ruby/Gems/1.8"
    path=("$GEM_HOME/bin" $path)
  fi
fi

# Return if requirements are not found.
if (( ! $+commands[ruby] && ! ( $+commands[rvm] || $+commands[rbenv] ) )); then
  return 1
fi

# Ruby GC tuning
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000

export IRBRC="$HOME/.irbrc"

export GEM_EDITOR="$GIT_EDITOR"

# Disable all Guard notifications
export GUARD_NOTIFY='false'

# Run Rubinius in 1.9 mode, and store rbc files in /tmp/rbx
export RBXOPT='-X19 -Xrbc.db=/tmp/rbx'

# Use custom Java 7 install (http://goo.gl/MkCSa)
if [[ -d "/Library/Java/JavaVirtualMachines/jdk1.7.0_10.jdk/Contents/Home" ]]; then
  export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_10.jdk/Contents/Home"
  path=($JAVA_HOME/bin $path)
fi

#
# Aliases
#

# General
alias rb='ruby'

# Bundler
if (( $+commands[bundle] )); then
  alias rbb='bundle'
  alias rbbe='rbb exec'
  alias rbbi='rbb install --path vendor/bundle'
  alias rbbl='rbb list'
  alias rbbo='rbb open'
  alias rbbp='rbb package'
  alias rbbu='rbb update'
  alias rbbI='rbbi \
    && rbb package \
    && print .bundle       >>! .gitignore \
    && print vendor/bundle >>! .gitignore \
    && print vendor/cache  >>! .gitignore'

  alias be='bundle exec'
  alias bc='bundle console'
  alias bch='bundle check'
  alias bi='bundle install'
  alias bu='bundle update'
  alias bs='bundle show'

  # Start Guard using bundle exec
  alias beg='bundle exec guard'
fi

# Tail the test log, ignoring TRUNCATE output from database_cleaner
alias rlog='tail -f log/test.log | grep -v TRUNCATE | grep -v "ALTER TABLE"'

# Use Nailgun to run some Ruby
alias rbng='ruby --ng -S'

# if (( $+commands[drip] )); then
#   export JAVACMD=$commands[drip]
#   export DRIP_INIT_CLASS=org.jruby.main.DripMain
# fi

# Settings from: https://github.com/jruby/jruby/wiki/Improving-startup-time
export JRUBY_OPTS="-J-XX:+TieredCompilation -J-XX:TieredStopAtLevel=1 -J-noverify"

# Guess JRUBY_HOME for things like vert.x
if [[ -d "$HOME/.rbenv/versions/jruby-1.7.4" ]]; then
  export JRUBY_HOME="$HOME/.rbenv/versions/jruby-1.7.4"
fi

if (( $+commands[foreman] )); then
  alias fs='foreman start'
fi

# Workaround for Ruby not being able to validate SSL certificates.
#
# To get the crt `brew install curl-ca-bundle`.
if [[ -f /usr/local/opt/curl-ca-bundle/share/ca-bundle.crt ]]; then
  export SSL_CERT_FILE=/usr/local/opt/curl-ca-bundle/share/ca-bundle.crt
fi
