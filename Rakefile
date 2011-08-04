require 'rake'

$replace_all = false

module Installer
  extend self

  CLEAR      = "\e[0m"
  GREEN      = "\e[32m"
  YELLOW     = "\e[33m"

  def say(message, color = nil)
    color = const_get(color.to_s.upcase) if color.is_a?(Symbol)
    puts "#{color}#{message}#{CLEAR}"
  end

  def interactive_link(file, destination, options = {})
    file_name        = File.basename(file)
    file_name        = ".#{file_name}" unless options[:dot] == false
    source_file      = File.join(Installer.dot_files, file)

    destination_file = File.expand_path(File.join(destination, file_name))

    if File.exist?(destination_file) || File.symlink?(destination_file)
      if File.identical?(source_file, destination_file)
        say "identical #{destination_file}"
        return
      end

      if $replace_all
        replace_file(destination_file, source_file)
      else
        print "overwrite #{destination_file}? [ynaq] "
        case $stdin.gets.chomp.downcase
        when 'a'
          $replace_all = true
          replace_file(destination_file, source_file)
        when 'y'
          replace_file(destination_file, source_file)
        when 'q'
          exit
        else
          say "skipping #{destination_file}", :yellow
        end
      end
    else
      link_file(destination_file, source_file)
    end
  end

  def dot_files
    File.dirname(__FILE__)
  end

  def replace_file(old_file, new_file)
    system %Q{rm "#{old_file}"}
    link_file(old_file, new_file)
  end

  def link_file(old_file, new_file)
    say("#{old_file} => #{new_file}", :green)
    system %Q{ln -fs "#{new_file}" "#{old_file}"}
  end
end

task :default => :install

desc "install the dot files into user's home directory"
task :install do
  # Setup vim swap directory
  File.mkdir(File.expand_path('~/.swp')) rescue nil

  files = %w(
    bin
    zsh/zshrc
    zsh/zshenv
    bash/bashrc
    bash/bash_profile
    misc/ackrc
    misc/inputrc
    misc/nanorc
    misc/ctags
    js
    ruby/autotest
    ruby/gemrc
    ruby/irbrc
    ruby/rvmrc
    ruby/railsrc
    ruby/rdebugrc
    git/gitk
    git/gitconfig
    git/gitignore
    git/gitattributes
    hg/hgrc
    vim
    vim/gvimrc
    vim/vimrc
  )

  files = Hash[files.zip(Array.new(files.size, "~/"))]

  files.each do |file, destination|
    Installer.interactive_link(file, destination)
  end

  Installer.interactive_link('ssh/config', '~/.ssh/', :dot => false)
  Installer.interactive_link('ruby/global.gems', '~/.rvm/gemsets/', :dot => false)
end
