#!/bin/bash

# owner
REPO_OWNER="leandropincini"

set -e

echo "Installing dotfiles:"
cp -v ./home/* ~/.dotfiles/

for i in ~/.dotfiles/*; do
	echo "Installing $(basename $i)..."
	rm -f ~/.$i
	ln -s $i ~/.$(basename $i)
done

