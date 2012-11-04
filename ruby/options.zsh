# Ruby GC tuning
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000

export IRBRC="$HOME/.irbrc"

export GEM_EDITOR="$GIT_EDITOR"

# Disable all Guard notifications
export GUARD_NOTIFY='false'

# Run Rubinius in 1.9 mode, and store rbc files in /tmp/rbx
export RBXOPT='-X19 -Xrbc.db=/tmp/rbx'

# Use custom Java 7 install (http://goo.gl/MkCSa)
if [[ -d "/Library/Java/JavaVirtualMachines/jdk1.7.0_07.jdk/Contents/Home" ]]; then
  export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_07.jdk/Contents/Home"
  path=($JAVA_HOME/bin $path)
fi

# Use 32-bit mode when running Java/JRuby
export JAVA_OPTS='-d32'
