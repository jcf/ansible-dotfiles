# Use the client JVM (http://j.mp/NHv51E)
export JAVA_OPTS='-d32'

# Ruby JRuby in 1.9 mode
export JRUBY_OPTS='--1.9'

# Use custom Java 7 install (http://goo.gl/P8gO1)
if [[ -d "/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home" ]]; then
  export JAVA_HOME="/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home"
fi
