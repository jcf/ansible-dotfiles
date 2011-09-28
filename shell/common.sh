PATH="$HOME/.bin:/usr/local/bin:/usr/local/sbin:/usr/X11/bin:/usr/bin:/usr/sbin:/bin:/sbin:${PATH}"
CDPATH=".:${HOME}"

source $DOT_FILES/$CURRENT_SHELL/config.*sh
source $DOT_FILES/shell/exports.sh
source $DOT_FILES/shell/aliases.sh
source $DOT_FILES/shell/functions.sh
source $DOT_FILES/shell/save-directory.sh
source $DOT_FILES/shell/git.sh

[[ -f $DOT_FILES/shell/private.sh ]] && source $DOT_FILES/shell/private.sh

[[ `uname -s` == 'Darwin' ]] && source $DOT_FILES/shell/osx.sh

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
