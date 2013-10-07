if (( ! $+commands[lein] )); then
  return 1
fi

alias cl='lein'
alias clr='lein repl'
alias cls='lein trampoline ring server-headless'
