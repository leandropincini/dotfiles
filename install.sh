#!/bin/bash

echo ">>> Installing dotfiles:"
cp -v ./home/* ~/.dotfiles/

for i in ~/.dotfiles/*; do
	echo "Installing $(basename $i)..."
	rm -f ~/.$(basename $i)
	ln -s $i ~/.$(basename $i)
done

echo ">>> Installing osx prefs"
bash ./scripts/osx

