#!/usr/bin/env bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ${HOME}/local/src/dotfiles
# And also installs Homebrew Packages and Anaconda
############################

# dotfiles directory
dotfiledir=${HOME}/local/src/dotfiles

# list of files/folders to symlink in ${homedir}
files="bash_profile bashrc bash_prompt aliases paths"

# change to the dotfiles directory
echo "Changing to the ${dotfiledir} directory"
cd ${dotfiledir}
echo "...done"

# create symlinks (will overwrite old dotfiles)
for file in ${files}; do
    echo "Creating symlink to $file in home directory."
    ln -sf ${dotfiledir}/.${file} ${HOME}/.${file}
done

# Download Git Auto-Completion
curl "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash" > ${HOME}/.git-completion.bash

# Run the Homebrew Script
./brew.sh

# Now create symlink to emacs file
ln -sf ${dotfiledir}/.emacs ${HOME}/.emacs.d/init.el

# Install miniconda
curl -s https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh | bash

# Install autojump
git clone git://github.com/wting/autojump.git && cd autojump && ./install.py
