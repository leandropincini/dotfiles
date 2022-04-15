#!/bin/bash
env RCRC=/dev/null
rcup -t archlinux -t global -x README.md -x docs -x home -x install-old.sh -x install-archlinux.sh -x install-macos.sh -x scripts -x toolbox -x .git -x .gitconfig -x .editorconfig -B valholl -q
if [ -f ~/.config/zsh/.zshrc ]; then
    rm ~/.config/zsh/.zshrc
fi
ln -sv ~/.config/zsh/zshrc ~/.config/zsh/.zshrc
