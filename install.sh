#!/bin/bash

set -e

echo ">>> Installing dotfiles:"
cp -v ./home/* ~/.dotfiles/

for i in ~/.dotfiles/*; do
	echo "Installing $(basename $i)..."
	rm -f ~/.$(basename $i)
	ln -s $i ~/.$(basename $i)
done

if [ $(uname) = "Darwin" ]; then
echo ">>> Installing osx prefs"
bash ./scripts/osx.sh
echo "Done. Note that some of these changs require a logout/restart to take effect."
fi

