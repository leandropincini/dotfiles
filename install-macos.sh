#!/bin/bash
env RCRC=/dev/null
rcup -t macos -t global -x README.md -x docs -x home -x install-old.sh -x install-archlinux.sh -x install-macos.sh -x scripts -x toolbox -x .git -x .gitconfig -x .editorconfig -B tinydancer -q
