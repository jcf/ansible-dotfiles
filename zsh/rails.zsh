# Rails
function rails_command {
  local cmd=$1
  shift
  if [ -e script/rails ]; then
    rails $cmd "$@"
  elif [ -e script/$cmd ]; then
    script/$cmd "$@"
  else
    echo "Rails command $cmd not found"
  fi
}

function rs { rails_command "server" "$@" }
function rc { rails_command "console" "$@" }
function rg { rails_command "generate" "$@" }
function rdb { rails_command "dbconsole" "$@" }
