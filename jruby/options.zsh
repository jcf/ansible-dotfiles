# Ruby JRuby in 1.9 mode
export JRUBY_OPTS='--1.9'

# Use custom Java 7 install (http://goo.gl/MkCSa)
if [[ -d "/Library/Java/JavaVirtualMachines/jdk1.7.0_07.jdk/Contents/Home" ]]; then
  export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_07.jdk/Contents/Home"
  path=($JAVA_HOME/bin $path)
fi
