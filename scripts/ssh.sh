#!/bin/bash

set -e

echo ">>> Installing ssh configs..."
if [ ! -d ~/.ssh ]; then
	mkdir ~/.ssh
fi

cp -v ./home/ssh/config ~/.ssh/config

