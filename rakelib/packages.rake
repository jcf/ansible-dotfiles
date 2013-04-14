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

  task :rbenv_plugins do
    %w( git://github.com/sstephenson/ruby-build.git
        git://github.com/tpope/rbenv-ctags.git
        git://github.com/tpope/rbenv-readline.git ).each do |url|
      system "git clone #{url} ~/.rbenv/plugins/#{File.basename(url, '.git')}"
    end
  end

  task :heroku_plugins do
    %w( git://github.com/ddollar/heroku-accounts.git
        https://github.com/tpope/heroku-binstubs.git
        https://github.com/tpope/heroku-wildcards.git
        https://github.com/tpope/heroku-remote.git
        https://github.com/tpope/heroku-surrogate.git
        https://github.com/tpope/heroku-pgbackups-pull.git ).each do |url|
      system "heroku plugins:install #{url}"
    end
  end

  desc 'Install dev dependencies'
  task :install => [:taps, :homebrew, :rubygems]
end
