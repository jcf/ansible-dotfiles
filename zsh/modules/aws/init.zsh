if [[ ! -d "$DOT/priv/aws" ]]; then
  return 1
fi

aws init
