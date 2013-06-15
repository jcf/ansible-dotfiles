# Dotfiles for Vim

## Structure

Everything is kept in vim.symlink. This directory is linked to from
`~/.vim` when you run `rake install`.

Any vim files in vim.symlink/config will be loaded automatically when
Vim starts up.

### Config files

- **00-bundles.vim** contains all instructions for NeoBundle.
- **05-gui.vim** contains all configuration that will be loaded only in
  GUI mode.
- **05-terminal.vim** contains configuration that will be loaded for
  both GUI and terminal Vim.
- **10-mappings.vim** contains all custom keymapping.
- **50-autocommands.vim** modifies Vims behaviour based on current
  state.
- **60-plugins.vim** configures Vim and installed plugins.

## Plugins

**Plugins are managed via [NeoBundle][]**. That means dependencies
can be installed and updated interactively.

After installing a dependency you have to tell NeoBundle about it
next time Vim starts up. I do this via my [00-bundles.vim][bundle-config]
file.

With a plugin added to Vundle via the `Bundle` command, you
can update it using `:BundleInstall!`.

For more information check out Vundle.

[NeoBundle]: https://github.com/Shougo/neobundle.vim
[bundle-config]: https://github.com/jcf/dotfiles/blob/master/vim/vim.symlink/config/00-bundles.vim

# Plugin List

Generated automatically with `rake plugins:update_readme`.


 * [vundle](https://github.com/gmarik/vundle) - Vundle, the plug-in manager for Vim
 * [vim-ruby](https://github.com/vim-ruby/vim-ruby) - Vim/Ruby Configuration Files
 * [vim-rake](https://github.com/tpope/vim-rake) - rake.vim: it's like rails.vim without the rails
 * [vim-bundler](https://github.com/tpope/vim-bundler) - bundler.vim: Lightweight support for Ruby's Bundler
 * [vim-endwise](https://github.com/tpope/vim-endwise) - endwise.vim: wisely add "end" in ruby, endfunction/endif/more in vim script, etc
 * [vim-rails](https://github.com/tpope/vim-rails) - rails.vim: Ruby on Rails power tools
 * [vim-ruby-sinatra](https://github.com/hallison/vim-ruby-sinatra) - Vim syntax highlight and snippets that helper coding applications based in Sinatra micro-framework.
 * [html-template-syntax](https://github.com/pbrisbin/html-template-syntax) - A set of vim syntax files for highlighting the various Html templating languages in Haskell
 * [PickAColor.vim](https://github.com/vim-scripts/PickAColor.vim) - Inserts and edits color codes using a color name or a graphic color chooser.
 * [colorv.vim](https://github.com/Rykka/colorv.vim) - A powerful color tool.
 * [vim-ragtag](https://github.com/tpope/vim-ragtag) - ragtag.vim: ghetto HTML/XML mappings (formerly allml.vim)
 * [hammer.vim](https://github.com/matthias-guenther/hammer.vim) - vim,  your markup language of choice, and your browser of choice.
 * [tube.vim](https://github.com/gcmt/tube.vim) - MacVim and iTerm/Terminal interaction without leaving MacVim
 * [go.vim](https://github.com/jcf/go.vim) - Go support on Vim (based on ${GOROOT}/misc/vim), with omnicompletion provided by gocode.
 * [vim-stylus](https://github.com/wavded/vim-stylus) - Syntax Highlighting for Stylus
 * [vim-slim](https://github.com/slim-template/vim-slim) - A clone of the slim vim plugin from stonean. For use with Pathogen.
 * [vim-haml](https://github.com/tpope/vim-haml) - Vim runtime files for Haml, Sass, and SCSS
 * [vim-less](https://github.com/groenewege/vim-less) - vim syntax for LESS (dynamic CSS)
 * [vim-coffee-script](https://github.com/kchmck/vim-coffee-script) - CoffeeScript support for vim
 * [vim-javascript](https://github.com/pangloss/vim-javascript) - Vastly improved vim's javascript indentation.
 * [vim-jade](https://github.com/statianzo/vim-jade) - Vim syntax highlighting for the Jade templating engine.
 * [textile.vim](https://github.com/timcharper/textile.vim) - Textile for VIM
 * [vim-liquid](https://github.com/tpope/vim-liquid) - Vim Liquid runtime files with Jekyll enhancements
 * [vim-markdown](https://github.com/tpope/vim-markdown) - Vim Markdown runtime files
 * [vim-jquery](https://github.com/itspriddle/vim-jquery) - Fork of bronson's vim-jquery which is now gone
 * [vim-handlebars](https://github.com/nono/vim-handlebars) - Vim plugin for Handlebars
 * [nginx.vim](https://github.com/mutewinter/nginx.vim) - Syntax highlighting for nginx.conf and related config files.
 * [vim-css3-syntax](https://github.com/hail2u/vim-css3-syntax) - Add CSS3 syntax support to vim's built-in `syntax/css.vim`.
 * [vim-fugitive](https://github.com/tpope/vim-fugitive) - fugitive.vim: a Git wrapper so awesome, it should be illegal
 * [vim-git](https://github.com/tpope/vim-git) - Vim Git runtime files
 * [gitv](https://github.com/gregsexton/gitv) - gitk for Vim.
 * [applescript.vim](https://github.com/vim-scripts/applescript.vim) - Syntax highlighting for AppleScript
 * [vim-cucumber](https://github.com/tpope/vim-cucumber) - Vim Cucumber runtime files
 * [puppet-vim](https://github.com/ajf/puppet-vim) - Vim stuff for puppet
 * [cocoa.vim](https://github.com/jcf/cocoa.vim) - Vim plugin for Cocoa/Objective-C development.
 * [vim-latex](https://github.com/jcf/vim-latex) - Mirror of vim-latex as Sourceforge's git support blows chunks!
 * [vim-json](https://github.com/leshill/vim-json) - Pathogen friendly packaging of vim-json from Jeroen Ruigrok van der Werven http://www.vim.org/scripts/script.php?script_id=1945
 * [vimerl](https://github.com/jimenezrick/vimerl) - The Erlang plugin for Vim
 * [haskellmode-vim](https://github.com/lukerandall/haskellmode-vim) - An unpacked copy of the haskellmode vimball. Ping me if it needs updating.
 * [VimClojure](https://github.com/vim-scripts/VimClojure) - A filetype, syntax and indent plugin for Clojure
 * [tomdoc.vim](https://github.com/duwanis/tomdoc.vim) - A simple syntax add-on for vim that highlights your TomDoc comments.
 * [vim-tomdoc](https://github.com/jc00ke/vim-tomdoc) - Simple vim plugin that adds TomDoc templates to your code.
 * [vim-colors-solarized](https://github.com/altercation/vim-colors-solarized) - precision colorscheme for the vim text editor
 * [vim-distinguished](https://github.com/Lokaltog/vim-distinguished) - A dark vim color scheme for 256-color terminals.
 * [vim-vividchalk](https://github.com/tpope/vim-vividchalk) - vividchalk.vim: a colorscheme strangely reminiscent of Vibrant Ink for a certain OS X editor
 * [bufexplorer](https://github.com/corntrace/bufexplorer) - Clone of Jeff Lanzarotta's bufexplorer from vim.org
 * [vim-rbenv](https://github.com/tpope/vim-rbenv) - rbenv.vim: Minimal rbenv support
 * [vim-obsession](https://github.com/tpope/vim-obsession) - obsession.vim: continuously updated session files
 * [nerdtree](https://github.com/scrooloose/nerdtree) - A tree explorer plugin for vim.
 * [splice.vim](https://github.com/sjl/splice.vim) - A Vim plugin for managing three-way merges.
 * [vim-scratch](https://github.com/duff/vim-scratch) - Yegappan Lakshmanan's scratch.vim plugin
 * [vimproc.vim](https://github.com/Shougo/vimproc.vim) - Interactive command execution in Vim.
 * [unite.vim](https://github.com/Shougo/unite.vim) - Unite and create user interfaces
 * [vimfiler.vim](https://github.com/Shougo/vimfiler.vim) - Powerful file explorer implemented by Vim script
 * [vimwiki](https://github.com/vim-scripts/vimwiki) - Personal Wiki for Vim
 * [unite-mark](https://github.com/tacroe/unite-mark)
 * [unite-git_grep](https://github.com/sgur/unite-git_grep) - git-grep source for unite.vim inspired by http://subtech.g.hatena.ne.jp/secondlife/20080606/1212729424
 * [unite-locate](https://github.com/ujihisa/unite-locate)
 * [unite-tag](https://github.com/tsukkee/unite-tag) - tags soruce for unite.vim
 * [unite-help](https://github.com/tsukkee/unite-help) - help source for unite.vim
 * [matchit.zip](https://github.com/vim-scripts/matchit.zip) - extended % matching for HTML, LaTeX, and many other languages
 * [vim-jump-to-code](https://github.com/jcf/vim-jump-to-code)
 * [powerline](https://github.com/Lokaltog/powerline) - The ultimate statusline/prompt utility.
 * [neocomplcache.vim](https://github.com/Shougo/neocomplcache.vim) - Ultimate auto-completion system for Vim.
 * [IndexedSearch](https://github.com/vim-scripts/IndexedSearch) - shows  'Nth match out of M'  at every search (index of match+total # matches)
 * [vim-dispatch](https://github.com/tpope/vim-dispatch) - dispatch.vim: asynchronous build and test dispatcher
 * [LargeFile](https://github.com/vim-scripts/LargeFile) - Edit large files quickly (keywords: large huge speed)
 * [golden-ratio](https://github.com/roman/golden-ratio) - Automatic resizing of Vim windows to the golden ratio 
 * [vim-indent-object](https://github.com/michaeljsmith/vim-indent-object) - Vim plugin that defines a new text object representing lines of code at the same indent level. Useful for python/vim scripts, etc.
 * [vim-space](https://github.com/spiiph/vim-space) - space.vim - Smart Space key for Vim
 * [ultisnips](https://github.com/SirVer/ultisnips) - Official Mirror of UltiSnips trunk on LaunchPad. Send pull requests to SirVer/ultisnips!
 * [file-line](https://github.com/bogado/file-line) - Plugin for vim to enabling opening a file in a given line
 * [gundo.vim](https://github.com/sjl/gundo.vim) - A git mirror of gundo.vim
 * [vim-easymotion](https://github.com/Lokaltog/vim-easymotion) - Vim motions on speed!
 * [vim-abolish](https://github.com/tpope/vim-abolish) - abolish.vim: easily search for, substitute, and abbreviate multiple variants of a word
 * [syntastic](https://github.com/scrooloose/syntastic) - Syntax checking hacks for vim
 * [vim-commentary](https://github.com/tpope/vim-commentary) - commentary.vim: comment stuff out
 * [ack.vim](https://github.com/mileszs/ack.vim) - Vim plugin for the Perl module / CLI script 'ack'
 * [ag.vim](https://github.com/epmatsw/ag.vim) - Vim plugin for the CLI script 'ag'
 * [vim-eunuch](https://github.com/tpope/vim-eunuch) - eunuch.vim: helpers for UNIX
 * [vim-repeat](https://github.com/tpope/vim-repeat) - repeat.vim: enable repeating supported plugin maps with "."
 * [vim-speeddating](https://github.com/tpope/vim-speeddating) - speeddating.vim: use CTRL-A/CTRL-X to increment dates, times, and more
 * [vim-surround](https://github.com/tpope/vim-surround) - surround.vim: quoting/parenthesizing made simple
 * [vim-unimpaired](https://github.com/tpope/vim-unimpaired) - unimpaired.vim: pairs of handy bracket mappings
 * [tabular](https://github.com/godlygeek/tabular) - Vim script for text filtering and alignment
 * [indentLine](https://github.com/Yggdroot/indentLine) - A vim plugin to display the indention levels with thin vertical lines
 * [ZoomWin](https://github.com/vim-scripts/ZoomWin) - Zoom in/out  of windows (toggle between one window and multi-window)
 * [sudo.vim](https://github.com/vim-scripts/sudo.vim) - Allows one to edit a file with prevledges from an unprivledged session.
 * [vim-gnupg](https://github.com/jamessan/vim-gnupg) - This script implements transparent editing of gpg encrypted files. The filename must have a ".gpg", ".pgp" or ".asc" suffix. When opening such a file the content is decrypted, when opening a new file the script will ask for the recipients of the encrypted file. The file content will be encrypted to all recipients before it is written. The script turns off viminfo, swapfile, and undofile to increase security. 
 * [switch.vim](https://github.com/AndrewRadev/switch.vim) - A simple Vim plugin to switch segments of text with predefined replacements
 * [ref.vim](https://github.com/vim-scripts/ref.vim) - Integrated reference viewer.
 * [vim-ref-ri](https://github.com/taka84u9/vim-ref-ri) - A vim-ref and Unite.vim source for ri.

That's 92 plugins.
