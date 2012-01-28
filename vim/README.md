# Dotfiles for Vim

Plugins are managed via Unbundle, which offers support for loading
plugins based on filetype. That means no loading of Ruby bundles when
you're writing C.

Plugins that need to be available all the time can be found in
`vim/bundle`. Plugins that are specific to a filetype can be found in
`vim/ftbundle/:filetype`.

Configuration can be found in `vim/config`. All files in config are
sourced in the `vimrc`.
