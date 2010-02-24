require 'rake'
require 'fileutils'

class DotFiles

  def initialize
    @dry_run = false
  end

  def dry_run?
    @dry_run == true
  end

  def dry_run!
    @dry_run = true
  end

  # Soft link dotfiles in home directory
  def link_files(opts = {})
    f = dry_run? ? FileUtils::DryRun : FileUtils
    Dir['*'].each do |file|
      next if %w[ Rakefile README LICENSE emacs ].include?(file)
      link = File.expand_path(File.join("~", ".#{file}"))
      to = File.join(Dir.pwd, file)
      f.ln_s(to, link, :force => true)
    end
  end

  # Find a token and update gitconfig accordingly
  def github_api_token
    token = ENV['GITHUB_API_TOKEN'] || get_token
    git_config_lines = replace_github_token(token)
    return puts(git_config_lines) if dry_run?
    File.open(gitconfig_path, 'w') { |f| f.write(git_config_lines) }
  end

private

  # Use gets to ask for a token
  def get_token
    print "Please enter your GitHub API token: "
    $stdin.gets.strip
  end

  # Return the path to gitconfig
  def gitconfig_path
    File.join(Dir.pwd, 'gitconfig')
  end

  # Read gitconfig, update token setting and return lines
  def replace_github_token(token)
    File.open(gitconfig_path, 'r') do |file|
      file.readlines.map { |line| line.sub(/token = (.*)$/, "token = #{token}") }
    end.join
  end

end

dot_files = DotFiles.new

desc "Install the dot files into user's home directory"
task :files do
  dot_files.link_files
end

desc "Print what will actually be done during the install"
task :dry_run do
  dot_files.dry_run!
  dot_files.link_files
  puts "I'd normally update your GitHub token with what you enter now..."
  dot_files.github_api_token
end

desc "Replace the GitHub API token"
task :token do
  dot_files.github_api_token
end

desc "Install the dotfiles and replace the GitHub API token"
task :install do
  dot_files.link_files
  dot_files.github_api_token
end
