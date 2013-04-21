# dotfiles

### Handcrafted with all the love in the world.

# Setup

1. Install a recent version of vim with your favourite package manager.

   ``` shell
   brew install macvim
   ```

   Optionally install emacs as well.

   ``` shell
   brew install emacs --cocoa --use-git-head --HEAD
   ```

2. Install git, because if you don't have it you need it in your life.

    ``` shell
    brew install git
    ```

3. Clone the repo.

   ``` shell
   git clone git://github.com/jcf/dotfiles ~/.dotfiles

4. Link dotfiles and install all the best packages.

   ``` shell
   cd ~/.dotfiles
   rake install packages:install
   ```

5. Wipe out my ssh config. It's no use to you.

   ``` shell
   echo "" > ssh/config
   ```

6. Replace my name, email, and Github user in `git/gitconfig.symlink`
   with your details.

7. Remove my PGP key, and add your own.

   ``` shell
   gpg -a --export <KEY_ID> > key.asc
   ```

# Tasks

**Install packages via Homebrew:**

``` shell
rake packages:install
```

**Update plugin list:**

``` shell
rake plugins:update_readme
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
