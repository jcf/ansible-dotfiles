# Dotfiles

**Dotfiles** helps you get your dev environment up and running.

[![emacs-preview-thumb](http://f.cl.ly/items/3i0b34452D2e0W2X3m3L/emacs-screenshot-thumb.png)](http://cl.ly/image/1j2d0W0o2w0O)

## What's in the box?

The following repositories are pulled in and setup:

- [emacs.d][] holds all of my Evil Emacs config.
- [prezto][] configures zsh.
- [vimrc][] sets up Vim for those times when Emacs isn't available.

And there's pretty good support for the following languages:

- [Clojure][]
- [Emacs][]
- [Erlang][]
- [Go][]
- [Haskell][]
- [Javascript][] via [nvm][]
- [MacVim][]
- [Python][] via [pyenv][]
- [Ruby][] via [rbenv][]

Each feature/language is grouped using [Ansible][] roles, and can be
found in the [roles][] directory.

All the dotfiles that get linked into `$HOME` are grouped by language
or feature where possible, and failing that lumped into the `dotfiles`
role.

Roles look at the `vars` kept in [group_vars][] for things like
packages to install, and versions of certain runtime environments.

## Getting Started

You'll need the following dependencies before getting started.

- [Ansible][]
- [Homebrew][]
- [Xcode][]

At the time of writing it's possible to get all three like so:

``` sh
wget https://bootstrap.pypa.io/get-pip.py
sudo python get-pip.py

ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

xcode-select --install
```

## Secrets

[Ansible Vault][] is used to encrypt some information that isn't really
sensitive, but is private enough I can't responsibly share it publicly.

An example of state that is kept private is the list of repositories to clone
into `~/Code`. The `code` role makes use of `dotfiles.code.github_repos`, which
is an encrypted list stored in `roles/code/vars/main.yml`.

To enable passwordless application of playbooks the secret used to decrypt a
vault is stored in the OS X keychain, and looked up via [`security(1)`][]. The
keychain item is lookup by its name and account, which matches the image below.

![Keychain](http://f.cl.ly/items/3B3r1q3Q1Q1s0M1X200t/example-dotfiles-keychain.png)

There are a few executables to ease working with Ansible Vault.

- [`bin/play`][] runs `ansible-playbook` with with vault key provided
- [`bin/vault-edit`][] opens a vault with `emacsclient` and the key from
  `bin/vault-key`
- [`bin/vault-key`][] prints the key for Ansible to use

## Install

With the dependencies above installed you can clone the repo, and use
[Ansible][] to get everything setup!

``` sh
git clone git://github.com/jcf/dotfiles.git ~/.dotfiles
cd ~/.dotfiles

bin/install
bin/sudo-install
```

Note, as the name suggests, running `bin/sudo-install` will result in use of
`sudo`. You may be prompted to enter your password depending on your `sudoers`.

## Update

To update the installed packages and tools you can use `bin/update`,
which internally runs all Ansible tasks with the tag `update`.

[Ansible Vault]: http://docs.ansible.com/playbooks_vault.html
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
[`bin/vault-edit`]: https://github.com/jcf/dotfiles/blob/master/bin/vault-edit
[`bin/vault-key`]: https://github.com/jcf/dotfiles/blob/master/bin/vault-key
[`bin/vault-play`]: https://github.com/jcf/dotfiles/blob/master/bin/vault-play
[`security(1)`]: https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/security.1.html
[emacs.d]: https://github.com/jcf/emacs.d
[group_vars]: https://github.com/jcf/dotfiles/tree/master/group_vars
[nvm]: https://github.com/creationix/nvm
[prezto]: https://github.com/jcf/prezto
[pyenv]: https://github.com/yyuu/pyenv
[rbenv]: https://github.com/sstephenson/rbenv
[roles]: https://github.com/jcf/dotfiles/tree/master/roles
[vimrc]: https://github.com/jcf/vimrc
