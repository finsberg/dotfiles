# dotfiles directory
dotfiledir=${HOME}/local/src/dotfiles

# list of files/folders to symlink in ${homedir}
files="bash_profile bashrc bash_prompt aliases paths secrets"

# change to the dotfiles directory
echo "Changing to the ${dotfiledir} directory"
cd ${dotfiledir}
echo "...done"

# create symlinks (will overwrite old dotfiles)
for file in ${files}; do
    echo "Creating symlink to $file in home directory."
    ln -sf ${dotfiledir}/.${file} ${HOME}/.${file}
done

# Now create symlink to emacs file
mkdir -p ${HOME}/.emacs.d/
echo "Creating symlink to emacs file"
ln -sf ${dotfiledir}/.emacs ${HOME}/.emacs.d/init.el
