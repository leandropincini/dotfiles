#!/bin/bash

set -e

echo ">>> >>> vundle..."

if [ -f /usr/bin/git ]; then
	if [ -d ~/.vim/bundle/vundle ]; then
		rm -rf ~/.vim/bundle/vundle
	fi

	git clone --depth 1 https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

	cp -v -r ../../home/vim/vundle/* ~/.vim/bundle/

	vim -u ~/.vim/bundle/bundles.vim +BundleInstall +qall
fi

