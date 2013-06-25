#!/bin/bash

set -e

bash ./scripts/dotfiles.sh

bash ./scripts/ssh.sh

if [ $(uname) = "Darwin" ]; then
	bash ./scripts/osx/osx.sh
fi

bash ./scripts/vim/vim.sh

echo "Done. Note that some of these changes require a logout/restart to take
effect."

