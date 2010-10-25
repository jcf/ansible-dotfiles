#!/usr/bin/env ruby
# from http://errtheblog.com/posts/89-huba-huba

home = ENV['HOME']

Dir.chdir File.dirname(__FILE__) do
  dotfiles_dir = Dir.pwd.sub(home + '/', '')

  Dir['*'].each do |file|
    next unless File.extname(file).empty?
    target_name = file == 'bin' ? file : ".#{file}"
    target = File.join(home, target_name)
    unless File.exist? target
      system %[ln -vsf #{File.join(dotfiles_dir, file)} #{target}]
    end
  end
end
