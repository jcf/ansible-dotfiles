PATH="$HOME/.bin:/usr/local/bin:/usr/local/sbin:${PATH}"
CDPATH=".:${HOME}"

source $DOT_FILES/$CURRENT_SHELL/config.*sh
source $DOT_FILES/shell/exports.sh
source $DOT_FILES/shell/aliases.sh
source $DOT_FILES/shell/functions.sh
source $DOT_FILES/shell/save-directory.sh
source $DOT_FILES/shell/git.sh
source $DOT_FILES/shell/ruby.sh

[[ -f $DOT_FILES/shell/private.sh ]] && source $DOT_FILES/shell/private.sh

[[ `uname -s` == 'Darwin' ]] && source $DOT_FILES/shell/osx.sh
