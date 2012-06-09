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
  require 'open3'

  PACKAGES = %w(
    git git-extras ruby-build hub jsl ctags lorem graphviz postgresql
    mongodb redis memcached node rlwrap couchdb wget tree vimpager
  )

  HEAD_PACKAGES = %w(willgit)

  def brew_install(*args, packages)
    args.unshift('brew', 'install')
    args += Array(packages)

    puts "Spawing brew process with args #{args.inspect}"
    Process.spawn(*args)
  end

  desc 'Install dev dependencies'
  task :install do
    brew_install('--HEAD', '--override-system-vim', 'macvim')
    brew_install(PACKAGES)
    brew_install('--HEAD', HEAD_PACKAGES)

    Process.waitall.each do |pid, status|
      puts "Process #{pid} exited with status #{status.exitstatus}"
    end
  end
end
