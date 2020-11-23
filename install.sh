#!/usr/bin/env bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ${HOME}/local/src/dotfiles
# And also installs Homebrew Packages and Anaconda
############################

./symlinks.sh

# Download Git Auto-Completion
curl "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash" > ${HOME}/.git-completion.bash

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Run the Homebrew Script
./brew.sh

# Run Conda
./conda.sh

# Install autojump
git clone git://github.com/wting/autojump.git && cd autojump && ./install.py
