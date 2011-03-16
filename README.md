# Setup

## Homebrew

[This script][gist] will prompt for confirmation before it does anything:

    ruby -e "$(curl -fsSL https://gist.github.com/raw/323731/install_homebrew.rb)"

Afterwards, [install Xcode][xcode].

## MacVim

    brew install macvim --HEAD

## Git

    brew install git

# Installation

## Formulae And Other Dependencies

    ./deps.sh

## Finally Some Files

    git clone git://github.com/jcf/dotfiles ~/.dotfiles
    cd ~/.dotfiles
    ruby install.rb

[gist]:http://gist.github.com/323731
[xcode]:http://developer.apple.com/technologies/xcode.html
