# verify if python is already installed
if [ -f /usr/local/bin/python ]; then
    echo "Python is already installed..."

    # install up-to-date pip's distribute
    echo "Installing distribute with pip..."
    # deactivating require virtualenv ;)
    sudo pip install --upgrade distribute

    # intall virtualenv
    echo "Installing virtualenv with pip..."
    sudo pip install --upgrade virtualenv

    # install mkvirtualenvwrapper
    echo "Installing mkvirtualenvwrapper with pip..."
    sudo pip install --upgrade virtualenvwrapper

    # install ipython
    echo "Installing ipython with pip..."
    sudo pip install --upgrade ipython
else
    echo "Install Python via official package at https://www.python.org/downloads/"
fi
