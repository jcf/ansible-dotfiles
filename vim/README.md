# Dotfiles for Vim

**Plugins are managed via [Vundle][vundle]**. That means depedencies can
be installed and updated interactively.

After installing a dependency you have to tell [Vundle][vundle] about it
next time Vim starts up. I do this via my [00-vundle.vim][vundleconf]
file.

With a plugin added to [Vundle][vundle] via the `Bundle` command, you
can update it using `:BundleInstall!`.

For more information check out [Vundle][vundle].

![vundle]: https://github.com/gmarik/vundle
![vundleconf]: https://github.com/jcf/dotfiles/blob/master/vim/vim.symlink/config/00-vundle.vim
