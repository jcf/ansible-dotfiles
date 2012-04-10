export CURRENT_SHELL="zsh"
export DOT_FILES=$(dirname $(dirname $(readlink $HOME/.zshrc)))

# Common functionality between Zsh and Bash
source $DOT_FILES/shell/common.sh

fpath=(
  ${plugins:+${0:h}/plugins/${^plugins}/{functions,completions}(/FN)}
  ${0:h}/{functions,completions}(/FN)
  $fpath
)

# Load and initialize the completion system ignoring insecure directories.
autoload -Uz compinit && compinit -i

# My ZSH customisations
source $DOT_FILES/zsh/config.zsh
source $DOT_FILES/zsh/prompt.zsh
source $DOT_FILES/zsh/aliases.zsh

# Colours
source $DOT_FILES/zsh/colors.zsh

# Completion
source $DOT_FILES/zsh/completion.zsh

# Autoload Zsh functions.
autoload -Uz age
autoload -Uz zargs
autoload -Uz zcalc
autoload -Uz zmv

# Source plugins defined in ~/.zshrc.
for plugin in ${0:h}/plugins/*/init.zsh; do
  source $plugin
done

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
