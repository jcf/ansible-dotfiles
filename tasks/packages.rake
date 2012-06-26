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
require 'open3'

namespace :packages do
  GEMS = %w(
    consular
    consular-iterm
    pry
    cocoapods
    motion-cocoapods
  )

  TAPS = %w(
    adamv/alt
    homebrew/dupes
  )

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
    rebar
    redis
    rlwrap
    ruby-build
    siege
    tree
    vimpager
    wget
    zeromq
    zsh
  )

  HEAD_PACKAGES = %w(
    willgit
  )

  task :rubygems do
    Process.spawn('gem', 'install', *GEMS)
  end

  task :taps do
    TAPS.each { |tap| Process.spawn('brew', 'tap', tap) }
    Process.waitall
  end

  task :homebrew do
    def brew_install(*args, packages)
      args.unshift('brew', 'install')
      args += Array(packages)

      puts "Spawing brew process with args #{args.inspect}"
      Process.spawn(*args)
    end

    brew_install('--HEAD', '--override-system-vim', 'macvim')
    brew_install(PACKAGES)
    brew_install('--HEAD', HEAD_PACKAGES)
  end

  desc 'Install dev dependencies'
  task :install => [:taps, :homebrew, :rubygems] do
    Process.waitall.each do |pid, status|
      puts "Process #{pid} exited with status #{status.exitstatus}"
    end
  end
end
