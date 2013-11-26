if (( ! $+commands[lein] )); then
  return 1
fi

alias cl='lein'
alias clr='lein repl'
alias cls='lein trampoline ring server-headless'
alias clt='lein with-profile test midje'
alias clta='clt :autotest'
