if [[ ! -d /usr/local/go ]]; then
  return 1
fi

path=(/usr/local/go/bin $path)
