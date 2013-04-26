export AWS_CONFIG_DIR="$DOT/priv/aws"

if [[ ! -d $AWS_CONFIG_DIR ]]; then
  return 1
fi

aws init
