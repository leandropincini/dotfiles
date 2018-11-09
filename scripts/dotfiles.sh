#!/bin/bash

set -e

echo ">>> Installing dotfiles..."

if [ ! -d ~/.dotfiles ]; then
        mkdir ~/.dotfiles
fi

cp -v ./home/files/* ~/.dotfiles/

if [ $(uname) = "Darwin" ]; then
    cp -v ./home/osx_files/* ~/.dotfiles/
fi

if [ $(uname) = "Linux" ] && [ -f /usr/bin/dnf ]; then
    cp -v ./home/linux_files/* ~/.dotfiles/
fi

for i in ~/.dotfiles/*; do
        echo "Installing $(basename $i)..."
        rm -f ~/.$(basename $i)
        ln -s $i ~/.$(basename $i)
        chmod 600 $i
        chmod 700 ~/.$(basename $i)
done
