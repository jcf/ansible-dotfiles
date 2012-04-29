if [[ -s "${HOME}/.rbenv/bin" ]]; then
  rbenv_root="${HOME}/.rbenv"
else
  rbenv_root="/usr/local/rbenv"
  export RBENV_ROOT="$rbenv_root"
fi

export PATH="${rbenv_root}/bin:$PATH"
eval "$(rbenv init -)"
