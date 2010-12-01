#!/usr/bin/env ruby

if RUBY_VERSION < '1.8.7'
  class Symbol
    def to_proc
      Proc.new { |*args| args.shift.__send__(self, *args) }
    end
  end
end

# Lovely stuff scrooloose
# (NSFW) http://got-ravings.blogspot.com/2009/10/mainframe-molestation-with-looksee.html
# require 'looksee/shortcuts'

# module Kernel
#   alias_method :orig_methods, :methods

#   def methods(*args)
#     if caller.first =~ /\(irb\):\d+:in `irb_binding'/
#       lp(self, *args)
#     else
#       orig_methods
#     end
#   end
# end

class Object
  def local_methods(obj = self)
    (methods - Object.instance_methods).sort
  end

  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    puts `ri '#{method}'`
  end
end

def quick(repetitions = 100, &block)
  require 'benchmark'
  Benchmark.bmbm do |b|
    b.report { repetitions.times(&block) }
  end
  nil
end

def reset_irb
  exec $0
end

alias q exit

load File.dirname(__FILE__) + '/.railsrc' if $0 == 'irb' && ENV['RAILS_ENV']

# Interactive editor inspired by Giles Bowkett
require 'tempfile'

class InteractiveBuffer
  def initialize(path)
    @file = Tempfile.new(path)
  end

  def to_s
    @file.path
  end
end

def mvim(path = 'irb_interactive_buffer')
  @interactive_buffer ||= InteractiveBuffer.new(path)
  system("mvim -f -c 'set ft=ruby' #{@interactive_buffer}")
  Object.class_eval(`cat #{@interactive_buffer}`)
end
alias vi mvim
