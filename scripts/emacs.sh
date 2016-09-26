#!/bin/bash

set -e

echo ">>> Installing emacs dotfiles..."

if [ -f ~/.emacs ]; then
    rm ~/.emacs
fi

if [ -d ~/.emacs.d ]; then
    rm -rf ~/.emacs.d
fi

cp -rv ./home/emacs.d/ ~/.emacs.d/
chmod 700 ~/.emacs.d/
