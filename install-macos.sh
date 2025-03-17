#!/bin/bash
env RCRC=/dev/null
rcup -t macos \
     -t global \
     -x README.md \
     -x docs \
     -x home \
     -x apply.sh \
     -x install-old.sh \
     -x install-archlinux.sh \
     -x install-macos.sh \
     -x scripts \
     -x toolbox \
     -x flake.lock \
     -x flake.nix \
     -x .git \
     -x .gitconfig \
     -x .editorconfig \
     -B tinydancer -q

if [ -f ~/.config/zsh/.zshrc ]; then
    rm ~/.config/zsh/.zshrc
fi
ln -sv ~/.config/zsh/zshrc ~/.config/zsh/.zshrc
ln -sv '~/projects/dotfiles/tag-macos/Library/Application\ Support/GnuCash/gtk-3.0.css' '~/Library/Application Support/Gnucash/gtk-3.0.css'
