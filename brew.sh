#!/usr/bin/env bash

# Get Homebrew
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# Install Brew Packages
brew install git
brew install wget
brew install tree
brew install aspell
brew install libomp
brew install ssh-vault
brew install redis
brew install git-crypt
brew install gcc

# Delete old emacs (if any)
sudo rm -f /usr/bin/emacs
sudo rm -rf /usr/share/emacs

# Install MacOS Applications
brew cask install google-chrome
brew cask install firefox
brew cask install visual-studio-code
brew cask install github
brew cask install spotify
brew cask install google-backup-and-sync
brew cask install dropbox
brew cask install gimp
brew cask install vlc
brew cask install iterm2
brew cask install slack
brew cask install paraview
brew cask install docker
brew cask install latexit
brew cask install adobe-acrobat-reader
brew cask install mactex


# Install Source Code Pro Font
brew tap homebrew/cask-fonts
brew cask install font-source-code-pro

