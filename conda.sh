# install miniconda
curl -s https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh | bash

conda config --set auto_activate_base true

source ~/.zshrc

# install main condax
python -m pip install condax

# Install clis that we allways use
condax install black
condax install flake8
condax install mypy
condax install pandoc
condax install isort

# Install packages that we allways use
conda install ipython jupyter scipy numpy matplotlib