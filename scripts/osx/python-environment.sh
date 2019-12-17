# verify if python is already installed
if [ -f /usr/local/bin/python ]; then
    # update pip
    echo "Updating pip..."
    sudo pip install --upgrade pip

    # install up-to-date pip's distribute
    echo "Installing distribute with pip..."
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

    # deactivating require virtualenv ;)
    export PIP_REQUIRE_VIRTUALENV=false

    # install certifi for emacs tls
    pip install --upgrade --user certifi

    # activating require virtualenv again ;)
    export PIP_REQUIRE_VIRTUAL_ENV=true
else
    echo "Install Python via official package at https://www.python.org/downloads/"
fi
