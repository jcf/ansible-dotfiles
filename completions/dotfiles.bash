_dotfiles() {
  COMPREPLY=()
  local word="${COMP_WORDS[COMP_CWORD]}"

  if [ "$COMP_CWORD" -eq 1 ]; then
    COMPREPLY=( $(compgen -W "$(dotfiles commands)" -- "$word") )
  else
    local command="${COMP_WORDS[1]}"
    local completions="$(dotfiles completions "$command")"
    COMPREPLY=( $(compgen -W "$completions" -- "$word") )
  fi
}

complete -F _dotfiles dotfiles
