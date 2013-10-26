#
# Configures Emacs dependency management.
#
# Authors: Sebastian Wiesner <lunaryorn@gmail.com>
#

# Enable Cask
if [[ -d "$HOME/.cask" ]]; then
  path=($HOME/.cask/bin $path)

  alias cai='cask install'
  alias cau='cask update'
  alias caI='cask init'
  alias cae='cask exec'
fi

# Open a file or the current directory in Emacs
function es {
  open -a /usr/local/Cellar/emacs/24.3/Emacs.app ${1-.}
}
