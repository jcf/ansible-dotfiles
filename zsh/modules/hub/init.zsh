# hub (http://github.com/defunkt/hub)
if (( ! $+commands[hub] )); then
  return 1
fi

function git() {
  hub "$@"
}
