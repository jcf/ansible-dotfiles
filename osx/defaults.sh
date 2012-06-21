# Disable press-and-hold for keys in favor of key repeat
defaults write -g ApplePressAndHoldEnabled -bool false

# Use AirDrop over every interface.
defaults write com.apple.NetworkBrowser BrowseAllInterfaces 1

# Always open everything in Finder's list view.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Show the ~/Library folder.
chflags nohidden ~/Library

# Use a flat dock.
defaults write com.apple.dock no-glass -boolean YES

# Show which apps are hidden by making the icon slightly transparent.
defaults write com.apple.Dock showhidden -bool YES

killall Dock
