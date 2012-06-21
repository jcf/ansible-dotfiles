# When using rbenv on a chef client with the rbenv cookbook from
# https://github.com/fnichol/chef-rbenv, this is the way to make sure rbenv is
# available within interactive shells.
#
# If this is not done, rbenv will not be available via PATH.
if [[ -s "${HOME}/.rbenv/bin" ]]; then
  rbenv_root="${HOME}/.rbenv"
else
  rbenv_root="/usr/local/rbenv"
  export RBENV_ROOT="$rbenv_root"
fi

export PATH="${rbenv_root}/bin:$PATH"
eval "$(rbenv init -)"
