if [[ ! -o interactive ]]; then
    return
fi

compctl -K _dotfiles dotfiles

_dotfiles() {
  local word words completions
  read -cA words
  word="${words[2]}"

  if [ "${#words}" -eq 2 ]; then
    completions="$(dotfiles commands)"
  else
    completions="$(dotfiles completions "${word}")"
  fi

  reply=("${(ps:\n:)completions}")
}
