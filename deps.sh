#!/usr/bin/env zsh

set -A base \
  git git-extras willgit growlnotify ruby-build hub jsl ctags lorem graphviz \
  postgresql mongodb redis memcached node rlwrap couchdb subversion wget tree

set -A head macvim

TRAPINT () {
  echo "Exiting..."
  exit
}

brew install $base
brew install --HEAD $head


if [[ -f ~/.nave ]]; then
  echo "Installing nave..."
  mkdir ~/.nave
  cd ~/.nave
  wget http://github.com/isaacs/nave/raw/master/nave.sh
  ln -s $PWD/nave.sh /usr/local/bin/nave
  chmod +x /usr/local/bin/nave
else
  echo "Nave already installed"
fi

echo "Installing latest version of node..."

/usr/local/bin/nave usemain `nave latest`
