# Dotfiles

**Dotfiles** helps you get your dev environment up and running.

## What's in the box?

- [Clojure][]
- [Emacs][]
- [Erlang][]
- [Go][]
- [Haskell][]
- [Javascript][] via [nvm][]
- [MacVim][]
- [Python][] via [pyenv][]
- [Ruby][] via [rbenv][]

## Getting Started

You'll need the following dependencies before getting started.

- [Ansible][]
- [Homebrew][]
- [Xcode][]

At the time of writing it's possible to get all three like so:

``` sh
sudo easy_install pip
sudo pip install ansible

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

xcode-select --install
```

## Install

With the dependencies above installed you can clone the repo, and use
[Ansible][] to get everything setup!

``` sh
git clone git://github.com/jcf/dotfiles.git ~/.dotfiles
cd ~/.dotfiles

bin/install

# This needs root access and will ask for your password
bin/sudo-install
```

## Update

To update the installed packages and tools you can use `bin/update`,
which internally runs all Ansible tasks with the tag `update`.

[Ansible]: http://www.ansible.com/
[Clojure]: http://clojure.org/
[Dotfiles]: https://github.com/jcf/dotfiles
[EVM]: https://github.com/rejeep/evm
[Emacs]: http://www.gnu.org/software/emacs
[Erlang]: http://www.erlang.org/
[Go]: http://golang.org/
[Haskell]: https://www.haskell.org/
[Homebrew]: http://brew.sh/
[Javascript]: https://www.destroyallsoftware.com/talks/wat
[MacVim]: https://code.google.com/p/macvim/
[Python]: https://www.python.org/
[Ruby]: https://www.ruby-lang.org/en/
[Xcode]: https://developer.apple.com/xcode/
[nvm]: https://github.com/creationix/nvm
[pyenv]: https://github.com/yyuu/pyenv
[rbenv]: https://github.com/sstephenson/rbenv
