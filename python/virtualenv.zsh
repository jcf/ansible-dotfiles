# virtualenvwrapper
#
# Installed via easy_install and pip as root:
#
#   sudo easy_install pip
#   sudo pip install virtualenv
#   sudo pip install virtualenvwrapper
if [[ -r /usr/local/bin/virtualenvwrapper.sh && -z $VIRTUALENVWRAPPER_PYTHON ]]; then
  source /usr/local/bin/virtualenvwrapper.sh
fi
