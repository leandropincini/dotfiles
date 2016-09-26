#!/bin/bash

set -e

echo ">>> Installing dotfiles..."

if [ ! -d ~/.dotfiles ]; then
	mkdir ~/.dotfiles
fi

cp -v ./home/files/* ~/.dotfiles/

for i in ~/.dotfiles/*; do
	echo "Installing $(basename $i)..."
	rm -f ~/.$(basename $i)
	ln -s $i ~/.$(basename $i)
	chmod 600 $i
	chmod 700 ~/.$(basename $i)
done

