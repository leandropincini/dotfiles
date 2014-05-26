#!/bin/bash

## To generate spell checker for vim:
#
# Download the latest version of pt_BR spell checker
#
# copy that at /tmp/pt_BR
# unzip with : unzip -x vero_pt_br_vxxx.oxt
#
# open vim : "vim ." in the /tmp/pt_BR
# run : mkspell pt-BR pt_BR

set -e

echo ">> >> pt_BR's spell checker.."

if [ ! -d ~/.vim/spell ]; then
	mkdir ~/.vim/spell
fi

cp -v -r ../../home/vim/spell/* ~/.vim/spell/

