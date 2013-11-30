# dotfiles

This repository will help you get setup with ZSH and decent development
environment for a number of programming languages.

There's a `dotfiles` executable that helps you install the packages that
you need to build great products, and keep your environment up-to-date.

Everything related to Z-Shell is in my fork of [Prezto][]. All things
zsh used to be vendored in this repo, but this meant a manual update
process any time new features were added to Prezto.

# Getting started

To get started you'll want to install [Xcode][] (installing Command Line
Tools is not enough to get MacVim installed), and [Homebrew][] as they
are required to install development dependencies .

Last time I checked installing Homebrew was as easy as running:

``` sh
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
```

## To the bat shell

With Homebrew installed you need to:

1. Install `git`.
2. Clone the dotfiles repo (optionally from your own fork).
3. Run `dotfiles setup`.
4. Purge dotfiles or `jcf`.

``` sh
# Install Homebrew
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

# Install git
/usr/local/bin/brew install git

# Clone your dotfiles file repo
/usr/local/bin/git clone git@github.com:<username>/dotfiles.git ~/.dotfiles

# Or if you're using my repo…
/usr/local/bin/git clone git@github.com:jcf/dotfiles.git ~/.dotfiles

# Activate the dotfiles executable
cd ~/.dotfiles
export PATH="./bin:$PATH"

# Run dotfiles setup
dotfiles setup

# Link the actual dotfiles in to your $HOME
dotfiles link
```

# Things I always do

### Restart Terminal

If you restart your Terminal of choice at this point you should find the
following ready to go:

- Prezto-influenced ZSH configuration.
- Required base packages like `hub`, `macvim` and the latest greatest
  `zsh`.
- Rbenv with some useful plugins.
- Pyenv.

### Download Vim plugins

Assuming you're going to use Vim you'll want to fire up your editor and
get NeoBundle to work.

```
vim -c 'NeoBundleInstall'
```

### Install all the things

`dotfiles install` is your friend. This will take a while depending on
your machine so expect to make a cup of tea before everything is
installed.

# Here be dragons

### Sudo make me a sandwich

Use `visudo` to allow all admins to use sudo without entering their
password.

``` sh
# Change this line:
%admin	ALL=(ALL) ALL

# To this:
%admin	ALL=(ALL) NOPASSWD: ALL
```

### Stop path_helper from messing with our path

``` sh
sudo chmod -x /usr/libexec/path_helper
```

### Switch to using Homebrew-installed ZSH

Edit `/etc/shells` as root and add `/usr/local/bin/zsh` to the end of
the file.

``` sh
# WARNING! I would highly recommend editing /etc/shells by hand!
grep '/usr/local/bin/zsh' /etc/shells > /dev/null || \
  sudo echo '/usr/local/bin/zsh' >> /etc/shells

# Change shell…
chsh -s /usr/local/bin/zsh
```

# License

Unless otherwise stated, everything is covered by the MIT license.

# Screenshots

<a href="http://cl.ly/image/1X2g2T1q1H2C">
  <img alt="iTerm 2" src="http://f.cl.ly/items/0v2X3F122p1i3g3K3g31/iterm2-thumb.png"></img>
</a>

<a href="http://cl.ly/image/2B253h193q0z">
  <img alt="MacVim" src="http://f.cl.ly/items/1m1M41221S233C0h0D2T/macvim.png"></img>
</a>

[Prezto]: https://github.com/jcf/prezto
[Xcode]: https://developer.apple.com/xcode/
[Homebrew]: https://github.com/mxcl/homebrew/wiki/Installation
