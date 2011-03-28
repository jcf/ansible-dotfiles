# Setup

## Homebrew

[This script][gist] will prompt for confirmation before it does anything:

    ruby -e "$(curl -fsSL https://gist.github.com/raw/323731/install_homebrew.rb)"

Afterwards, [install Xcode][xcode].

## MacVim

    brew install macvim # with or without --HEAD

## Git

    brew install git

# Installation

## Formulae And Other Dependencies

    ./deps.sh

## Finally Some Files

    git clone git://github.com/jcf/dotfiles ~/.dotfiles
    cd ~/.dotfiles
    rake install

# W(here)TF?

Most dotfiles can be found in a respective folder, for example, the `git`
and `ruby` folders contain files that are all symlinked in `$HOME`.

The `zsh` folder is slightly more involved. In here you have a `zshrc`
and `zshenv` file which are symlinked. The other files in this directory
are sourced from these files.

`shell` contains files that are used by both `zsh` and `bash`.

[gist]:http://gist.github.com/323731
[xcode]:http://developer.apple.com/technologies/xcode.html
