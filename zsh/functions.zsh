function GET() {
  curl -i -X GET -H "X-Requested-With: XMLHttpRequest" $*
}

function POST() {
  curl -i -X POST -H "X-Requested-With: XMLHttpRequest" $*
  #-d "key=val"
}

function PUT() {
  curl -i -X PUT -H "X-Requested-With: XMLHttpRequest" $*
}

function DELETE() {
  curl -i -X DELETE -H "X-Requested-With: XMLHttpRequest" $*
}

function f() { find * -name $1; }

function m() {
  file=.
  cd_to=.

  if [ -n "$*" ]; then
    if [ -d "$1" ]; then
      cd_to=$1
      file=.
    else
      file=$*
    fi
  fi

  eval "cd $cd_to && $VISUAL $file"
}

function extract() {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2) tar xvjf $1   ;;
      *.tar.gz)  tar xvzf $1   ;;
      *.bz2)     bunzip2 $1    ;;
      *.rar)     unrar x $1    ;;
      *.gz)      gunzip $1     ;;
      *.tar)     tar xvf $1    ;;
      *.tbz2)    tar xvjf $1   ;;
      *.tgz)     tar xvzf $1   ;;
      *.zip)     unzip $1      ;;
      *.Z)       uncompress $1 ;;
      *.7z)      7z x $1       ;;
      *)         echo "'$1' cannot be extracted via >extract<" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Dig everything
function dug() {
  dig +nocmd $1 any +multiline +noall +answer
}

function json() {
  tmpfile=`mktemp -t json`
  curl -s $* | python -mjson.tool > $tmpfile
  cat $tmpfile
  cat $tmpfile | pbcopy
  rm $tmpfile
}

function xml() {
  tmpfile=`mktemp -t xml`
  curl -s $* | xmllint —format - > $tmpfile
  cat $tmpfile
  cat $tmpfile | pbcopy
  rm $tmpfile
}
