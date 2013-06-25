#!/bin/bash

set -e

echo ">>> >>> vundle..."

if [ ! -f /usr/bin/git ]; then
	git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

	cp -v -r ../../home/vim/vundle/* ~/.vim/bundle/

	vim -u ~/.vim/bundle/bundles.vim +BundleInstall +qall
fi

