#!/bin/bash
env RCRC=/dev/null
rcup -t archlinux -t global -x README.md -x docs -x home -x install.sh -x scripts -x toolbox -x .git -x .gitconfig -x .gitignore -x .editorconfig -x rcstart-archlinux -x rcstart-macos -B valholl -q
if [ -f ~/.config/zsh/.zshrc ]; then
	rm ~/.config/zsh/.zshrc
fi
ln -sv ~/.config/zsh/zshrc ~/.config/zsh/.zshrc
