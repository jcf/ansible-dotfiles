if (( ! $+commands[mosh] )); then
  return 1
fi

# Useful when you've installed mosh using Brew and it mosh-server isn't in your
# PATH when you connect.
alias bosh="mosh --server=/usr/local/bin/mosh-server"
