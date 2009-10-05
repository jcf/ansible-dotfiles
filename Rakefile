require 'rake'
require 'fileutils'

class DotFiles

  attr_reader :dry_run

  def initialize
    @dry_run = false
  end

  def dry_run!
    @dry_run = true
  end

  def get_token
    print "Please enter your GitHub API token: "
    $stdin.gets.strip
  end

  def gitconfig_path
    File.join(Dir.pwd, 'gitconfig')
  end

  def replace_github_token(token)
    File.open(gitconfig_path, 'r') do |file|
      file.readlines.map { |line| line.sub(/token = (.*)$/, "token = #{token}") }
    end.join
  end

  def github_api_token(dryrun)
    token = ENV['GITHUB_API_TOKEN'] || get_token
    git_config_lines = replace_github_token(token)
    return puts(git_config_lines) if @dry_run
    File.open(gitconfig_path, 'w') { |f| f.write(git_config_lines) }
  end

  def link_files(opts = {})
    f = opts[:with] || FileUtils::DryRun
    Dir['*'].each do |file|
      next if %w[ Rakefile README LICENSE ].include?(file)
      link = File.expand_path(File.join("~", ".#{file}"))
      f.ln_s(file, link, :force => true)
    end
  end

end

dot_files = DotFiles.new

desc "Install the dot files into user's home directory"
task :install do
  dot_files.link_files(:with => FileUtils)
  dot_files.github_api_token
end

desc "Print what will actually be done during the install"
task :dry_run do
  dot_files.dry_run!

  dot_files.link_files
  puts "I'd normally replace your GitHub API token now..."
  dot_files.github_api_token(:dryrun)
end