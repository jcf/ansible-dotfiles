autoload zsh/terminfo

# format titles for screen and rxvt
function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%40>...>$a" | tr -d "\n")

  case $TERM in
  screen)
    print -Pn "\ek$a:$3\e\\"      # screen title (in ^A")
    ;;
  xterm*|rxvt)
    print -Pn "\e]0;$2 | $a:$3\a" # plain xterm title
    ;;
  esac
}

pr_reset="%f%u%k%s%b" # reset all codes

if [ "$terminfo[colors]" -eq 256 ]; then
  pr_red="%F{52}"
  pr_blue="%F{25}"
  pr_green="%F{28}"
  pr_grey="%F{59}"
else
  if [ "$terminfo[colors]" -eq 8 ]; then
    pr_red="%F{red}"
    pr_blue="%F{blue}"
    pr_green="%F{green}"
    pr_grey="%B%F{black}"
  fi
fi

# VCS configuration
autoload vcs_info
zstyle ':vcs_info:*'      enable             git hg svn
zstyle ':vcs_info:*'      get-revision       true
zstyle ':vcs_info:*'      formats            "(%s) %b:%8>>%i%<<" "%r"
zstyle ':vcs_info:*'      actionformats      "(%s) %i|%a"
zstyle ':vcs_info:*'      branchformat       "%b:%r"
zstyle ':vcs_info:hg*:*'  use-simple         true
zstyle ':vcs_info:svn:*'  formats            "(%s) %b:r%i" "%r"
zstyle ':vcs_info:svn:*'  branchformat       "%b"

function prompt_pwd() {
  local repo="$vcs_info_msg_1_"

  parts=(${(s:/:)${${PWD}/#${HOME}/\~}})

  i=0
  while (( i++ < ${#parts} )); do
    part="$parts[i]"
    if [[ "$part" == "$repo" ]]; then
      # if this part of the path represents the repo,
      # underline it, and skip truncating the component
      parts[i]="%U$part%u"
    else
      # Shorten the path as long as it isn't the last piece
      if [[ "$parts[${#parts}]" != "$part" ]]; then
        parts[i]="$part[1,1]"
      fi
    fi
  done

  echo "${(j:/:)parts}"
}

function precmd {
  vcs_info

  local cwd="$pr_blue`prompt_pwd`$pr_reset"
  local char="%0(?.$pr_green.$pr_red)$pr_reset"

  local user_at_host
  if [[ "$USER" != "jcf" ]]; then
    user_at_host="$USER"

    if [[ "$user" == "root" ]] then
      user_at_host="$pr_red$user_at_host$pr_reset"
    fi
  fi

  if [[ -n "$SSH_TTY" ]]; then
    user_at_host+="$pr_blue@`hostname -s`$pr_reset"
  fi

  title "zsh" "$USER@%m" "%55<...<%~"

  local rev="$pr_grey$vcs_info_msg_0_$pr_reset"
  rev="${rev/\(git\)/±}"
  rev="${rev/\(hg\)/☿}"
  rev="${rev/\(svn\)/↯}"

  local left right
  left=($user_at_host $cwd $char)
  right=($rev)

  PS1="$left"
  RPS1=" $right"
}

# preexec is called just before any command line is executed
function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}

