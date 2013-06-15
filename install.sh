#!/bin/bash

set -e

echo ">>> Installing dotfiles:"
cp -v ./home/files/* ~/.dotfiles/
chmod 700 ~/.dotfiles/

for i in ~/.dotfiles/*; do
	echo "Installing $(basename $i)..."
	rm -f ~/.$(basename $i)
	ln -s $i ~/.$(basename $i)
	chmod 600 $i
	chmod 700 ~/.$(basename $i)
done

# download the latest version of pt_BR spellscheck at
#
# http://extensions.libreoffice.org/extension-center/vero-verificador-ortografico-e-hifenizador-em-portugues-do-brasil
#
# copy that at ~/temp/pt_BR
# unzip with : unzip -x vero_pt_br_vxxx.oxt
#
# open vim : "vim ."
# run : mkspell pt-BR pt_BR

echo ">>> Installing vim configs..."
if [ ! -d ~/.vim/spell ]; then
	mkdir ~/.vim/spell
fi
cp -v -r ./home/vim/spell/* ~/.vim/spell/

echo ">>> Installing ssh configs..."
if [ ! -d ~/.ssh ]; then
	mkdir ~/.ssh
fi
cp -v ./home/ssh/config ~/.ssh/config

if [ $(uname) = "Darwin" ]; then
	echo ">>> Installing osx prefs..."
	bash ./scripts/osx.sh
	echo "Done. Note that some of these changs require a logout/restart to take effect."
fi

