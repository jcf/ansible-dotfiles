#!/usr/bin/env zsh

set -A base \
  git git-extras willgit git-flow growlnotify hub jsl ctags lorem graphviz \
  postgresql mongodb redis memcached node rlwrap couchdb subversion wget tree

set -A head macvim

TRAPINT () {
  echo "Exiting..."
  exit
}

brew install $base
brew install --HEAD $head
