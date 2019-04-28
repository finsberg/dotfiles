#!/usr/bin/env bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ${HOME}/local/src/dotfiles
# And also installs Homebrew Packages and Anaconda
############################

./symlinks.sh

# Download Git Auto-Completion
curl "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash" > ${HOME}/.git-completion.bash

# Run the Homebrew Script
./brew.sh

# Install miniconda
curl -s https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh | bash

# Install autojump
git clone git://github.com/wting/autojump.git && cd autojump && ./install.py
