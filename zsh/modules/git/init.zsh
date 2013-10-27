#
# Provides Git aliases and functions.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Return if requirements are not found
if (( ! $+commands[git] )); then
  return 1
fi

# Load dependencies
pmodload 'helper'

# Source module files
source "${0:h}/alias.zsh"

export GIT_EDITOR='emacsclient --quiet'

# hub (http://github.com/defunkt/hub)
if (( $+commands[hub] )); then
  function git() {
    hub "$@"
  }
fi

# Add git-extras to git completion
if (( $+commands['git-extras'] )); then
  zstyle ':completion:*:*:git:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
fi
