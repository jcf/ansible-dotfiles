# Rails
rails_command() {
  local cmd=$1
  shift
  if [ -e script/rails ]; then
    rails $cmd "$@"
  elif [ -e script/$cmd ]; then
    script/$cmd "$@"
  else
    echo Rails command "$cmd" not found
  fi
}

rs() {
  rails_command "server" "$@"
}

rc() {
  rails_command "console" "$@"
}

rg() {
  rails_command "generate" "$@"
}

rdb() {
  rails_command "dbconsole" "$@"
}
