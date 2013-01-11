# Installs packages I use regularly. Homebrew is required for this to
# work.
#
# Homebrew:
#   /usr/bin/ruby -e "$(/usr/bin/curl -fsSL https://raw.github.com/mxcl/homebrew/master/Library/Contributions/install_homebrew.rb)"
#
# rbenv:
#   git clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
#
# NPM:
#   curl http://npmjs.org/install.sh | sh

namespace :packages do
  PACKAGES = %w(
    ack
    couchdb
    ctags
    erlang
    git
    git-extras
    graphviz
    htop-osx
    hub
    jsl
    libxml2
    libxslt
    lorem
    memcached
    mongodb
    moreutils
    node
    phantomjs
    postgis
    postgresql
    pv
    redis
    rlwrap
    ruby-build
    siege
    tree
    vimpager
    wget
    zeromq
    zsh
    zsh-completions
  )

  HEAD_PACKAGES = %w(
    rebar
    willgit
  )

  GEMS = %w(
    cocoapods
    consular
    consular-iterm
    gem-browse
    gem-ctags
    motion-cocoapods
    pry
  )

  task :homebrew do
    system 'brew install --HEAD --override-system-vim macvim'
    system "brew install #{PACKAGES.join(' ')}"
    system "brew install --HEAD #{HEAD_PACKAGES.join(' ')}"
  end

  task :rubygems do
    system "gem install #{GEMS.join(' ')}"
  end

  task :heroku_plugins do
    system 'heroku plugins:install git://github.com/ddollar/heroku-accounts.git'
  end

  desc 'Install dev dependencies'
  task :install => [:taps, :homebrew, :rubygems]
end
