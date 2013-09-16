if (( ! $+commands[erl] )); then
  return 1
fi

alias erp='erl -pa ebin deps/*/ebin'
