#!/bin/bash

set -e

echo "Installing linux prefs..."

# ask for the administrator password upfront
sudo -v

# update existing 'sudo' time stamp until 'instalation' is finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# dnf update
sudo dnf update -y

# softwares
echo "Installing gnome-tweaks with dnf"
sudo dnf install gnome-tweaks -y

echo "Installing util-linux-user with dnf"
sudo dnf install util-linux-user -y

echo "Installing htop with dnf"
sudo dnf install htop -y

echo "Installing p7zip with dnf"
sudo dnf install p7zip p7zip-plugins -y

echo "Installing ripgrep with dnf"
sudo dnf install ripgrep -y

echo "Installing docker with dnf"
sudo dnf install docker docker-compose -y

echo "Installing emacs with dnf"
sudo dnf install emacs -y

# install a java environment
if [ -f ./scripts/linux/java-environment.sh ]; then
    if [ -f /usr/bin/java ]; then
        echo "Installing a java environment..."
        bash ./scripts/linux/java-environment.sh
    fi
fi

# install a zsh environment
#if [ -f ./scripts/linux/zsh.sh ]; then
#    echo "Installing a zsh enviroment..."
#    bash ./scripts/linux/zsh.sh
#fi
