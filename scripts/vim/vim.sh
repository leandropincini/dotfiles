#!/bin/bash

set -e

echo ">>> Installing vim configs..."

bash ./scripts/vim/spell.sh

bash ./scripts/vim/vundle.sh

