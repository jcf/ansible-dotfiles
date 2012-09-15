# Virtualenv Burrito & virtualenvwrapper
#
# https://github.com/brainsik/virtualenv-burrito
#
# virtualenvwrapper can be installed via easy_install and pip as root:
#
#   sudo easy_install pip
#   sudo pip install virtualenv
#   sudo pip install virtualenvwrapper
if [[ -r $HOME/.venvburrito/startup.sh ]]; then
  source $HOME/.venvburrito/startup.sh
elif [[ -r /usr/local/bin/virtualenvwrapper.sh && -z $VIRTUALENVWRAPPER_PYTHON ]]; then
  source /usr/local/bin/virtualenvwrapper.sh
fi
