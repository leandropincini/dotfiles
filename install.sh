#!/bin/bash

set -e

bash ./scripts/dotfiles.sh

#bash ./scripts/ssh.sh

#bash ./scripts/emacs.sh

#bash ./scripts/vim/vim.sh

if [ $(uname) = "Darwin" ]; then
    bash ./scripts/osx/osx.sh
fi

if [ $(uname) = "Linux" ]; then
    bash ./scripts/linux/linux.sh
fi

echo "Done. Note that some of these changes require a logout/restart to take effect."
