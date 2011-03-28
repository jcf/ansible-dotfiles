alias o='open .'

function manpdf() { man -t $@ | open -f -a Preview; }

function osinfo() { 
   x1="$(/usr/bin/sw_vers -productName)"
   x2="$(/usr/bin/sw_vers -productVersion)"
   x3="$(/usr/bin/sw_vers -buildVersion)"
   x4="$(/usr/bin/arch)"
   echo "${x1} - ${x2} - ${x3} - ${x4}"
}

function tab() {
  osascript 2>/dev/null <<EOF
    tell application "System Events"
      tell process "Terminal" to keystroke "t" using command down
    end
    tell application "Terminal"
      activate
      do script with command "cd \"$PWD\"; $*" in window 1
    end tell
EOF
}

# Minimise terminal window to Dock
function mintw() { printf "\e[2t"; return 0; }

# Send Terminal window to background
function bgtw() { printf "\e[6t"; return 0; }

function hidetw() { 
   /usr/bin/osascript -e 'tell application "System Events" to set visible of some item of ( get processes whose name = "Terminal" ) to false'
   return 0
}

# show number of lines & columns
function lc() { printf "lines: $(/usr/bin/tput lines)\ncolums: $(/usr/bin/tput cols)\n"; return 0; }
