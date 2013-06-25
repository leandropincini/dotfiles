#!/bin/bash

# Download the latest version of pt_BR spell checker at
#
# http://extensions.libreoffice.org/extension-center/vero-verificador-ortografico-e-hifenizador-em-portugues-do-brasil
#
# copy that at /tmp/pt_BR
# unzip with : unzip -x vero_pt_br_vxxx.oxt
#
# open vim : "vim ."
# run : mkspell pt-BR pt_BR

set -e

echo ">> >> pt_BR's spell checker.."

if [ ! -d ~/.vim/spell ]; then
	mkdir ~/.vim/spell
fi

cp -v -r ../../home/vim/spell/* ~/.vim/spell/

