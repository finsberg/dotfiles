#!/usr/bin/env bash

# Install Brew Packages
brew install python
brew install ipython
brew install flake8
brew install pandoc
brew install tree

# Delete old emacs (if any)
sudo rm -f /usr/bin/emacs
sudo rm -rf /usr/share/emacs

# Install MacOS Applications
brew cask install emacs
brew cask install google-chrome
brew cask install firefox
brew cask install sourcetree
brew cask install spotify
brew cask install google-backup-and-sync
brew cask install dropbox
brew cask install skype
brew cask install gimp
brew cask install vlc
brew cask install divvy
brew cask install mactex


# Install Source Code Pro Font
brew tap homebrew/cask-fonts
brew cask install font-source-code-pro

