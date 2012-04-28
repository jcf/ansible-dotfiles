#!/usr/bin/env zsh

# Use Homebrew to install useful executables
set -A base \
  git git-extras willgit growlnotify ruby-build hub jsl ctags lorem graphviz \
  postgresql mongodb redis memcached node rlwrap couchdb subversion wget tree \
  vimpager z

TRAPINT () {
  echo "Exiting..."
  exit
}

brew install $base
brew install macvim --override-system-vim

# Install PythonBrew
# curl -kL http://xrl.us/pythonbrewinstall | bash

# Install NPM
# curl http://npmjs.org/install.sh | sh

# Install rbenv
# git clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
