# Simplified dotfile for video recordings

# Load dotfiles:
for file in ~/.{bash_prompt,aliases,paths}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Git auto-complete
if [ -f ~/.git-completion.bash ]; then
    source ~/.git-completion.bash
fi

if [ -f ~/.secrets ]; then
    source ~/.secrets
fi

# Autojump
[[ -s ${HOME}/.autojump/etc/profile.d/autojump.sh ]] && source ${HOME}/.autojump/etc/profile.d/autojump.sh
