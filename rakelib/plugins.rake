# Taken from @mutewinter's dotfiles.
#
# https://github.com/mutewinter/dot_vim/blob/master/Rakefile
#
# Authors:
#   Jeremy Mack <http://pileofturtles.com>
#   James Conroy-Finn <james@logi.cl>
#
# Changes:
#
# - Rescue exceptions when connecting to Github.
# - Use Vundle config file rather than vimrc to find vundled plugins.
namespace :plugins do
  require 'open-uri'
  require 'openssl'
  require 'json'

  # TODO Refactor plugin list generation

  README_FILE = 'vim/README.md'
  VUNDLE_CONFIG = 'vim/vim.symlink/config/00-vundle.vim'

  PLUGIN_LIST_TAG = '# Plugin List'
  PLUGIN_LIST_NOTE = 'Generated automatically with `rake plugins:update_readme`.'

  def descriptions_from_plugins(plugins)
    plugins.map { |p| " * [#{p[:name]}](#{p[:uri]}) - #{p[:description]}" }
  end

  def add_plugins_to_readme(plugins = [])
    lines = File.readlines(README_FILE).map(&:chomp)
    index = lines.index(PLUGIN_LIST_TAG)

    if index
      lines.insert(index + 1, "\n#{PLUGIN_LIST_NOTE}\n\n")
      lines.insert(index + 2, descriptions_from_plugins(plugins))
      lines << "\nThat's #{plugins.length} plugins.\n"
      write_lines_to_readme(lines)
    else
      abort "Error: Plugin List Tag (#{PLUGIN_LIST_TAG}) not found"
    end
  end

  # TODO Prevent removal of lines after plugin list
  def delete_old_plugins_from_readme
    lines = []
    File.readlines(README_FILE).map do |line|
      line.chomp!
      lines << line
      if line == PLUGIN_LIST_TAG
        break
      end
    end

    write_lines_to_readme(lines)
  end

  def write_lines_to_readme(lines)
    File.open(README_FILE, 'w') do |io|
      io << lines.join("\n")
    end
  end

  # Returns an array of plugins denoted with Bundle
  def parse_plugins_from_vimrc
    File.new(VUNDLE_CONFIG).map do |line|
      if line =~ /Bundle\s+["'](.+)["']$/
        convert_to_link_hash($1)
      end
    end.compact
  end

  # Converts a Vundle link to a URI
  def convert_to_link_hash(link)
    link_hash = {}

    if link =~ /([a-zA-Z0-9\-]*)\/([a-zA-Z0-9\-\._]*)/
      user = $1
      name = $2
      link_hash[:user] = user
      link_hash[:name] = name
      link_hash[:uri] = "https://github.com/#{user}/#{name}"
      link_hash[:description] = fetch_github_repo_description(user, name)
    else
      name = link
      link_hash[:name] = name
      link_hash[:uri] = "https://github.com/vim-scripts/#{name}"
      link_hash[:description] = fetch_github_repo_description('vim-scripts', name)
    end

    link_hash
  end

  def credentials
    if ENV['GITHUB_CLIENT_ID'] && ENV['GITHUB_CLIENT_SECRET']
      "?client_id=#{ENV['GITHUB_CLIENT_ID']}&" \
        "client_secret=#{ENV['GITHUB_CLIENT_SECRET']}"
    else
      ''
    end
  end

  def fetch_github_repo_description(user, name)
    url = "https://api.github.com/repos/#{user}/#{name}"

    puts "Download repo info for #{url}..."
    response = open(url + credentials, ssl_verify_mode: OpenSSL::SSL::VERIFY_NONE).read

    JSON.parse(response)['description']

  rescue OpenURI::HTTPError
    $stderr.puts "Error while requesting Github repo info for #{url.inspect}"
    raise
  end

  desc 'Update the list of plugins in README.md'
  task :update_readme do
    plugins = parse_plugins_from_vimrc
    delete_old_plugins_from_readme
    add_plugins_to_readme(plugins)
  end
end
