#!/bin/bash

set -e

echo "Installing osx prefs..."

# ask for the administrator password upfront
sudo -v

# update existing 'sudo' time stamp until 'instalation' is finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# set computer name (as done via System Preferences > Sharing)
echo "Setting this machine name : valholl..."
sudo scutil --set ComputerName "valholl"
sudo scutil --set HostName "valholl"
sudo scutil --set LocalHostName "valholl"
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "valholl"

# save to disk (not to iCloud) by default
echo "Setting to save to disk (not to iCloud) by default"
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# automatically quit printer app once the print jobs complete
echo "Setting to automatically quit printer app once the print jobs complete..."
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# check for software update daily, not just once per week
echo "Setting to check for software update daily..."
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# finder: show all filename extensions
echo "Setting finder to show all filename extensions..."
defaults write com.apple.finder AppleShowAllExtensions -bool true

# finder: show status bar
echo "Setting finder to show status bar..."
defaults write com.apple.finder ShowStatusBar -bool true

# when performing a search, search the current folder by default
echo "Setting finder to search the current folde by default..."
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# avoid creating .DS_Store files on network volumes,
# don't work with external usb drivers
echo "Avoiding creating .DS_Store files on network volumes..."
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# empty trash securely by defult
echo "Setting to empty trash securely by default..."
defaults write com.apple.finder EmptyTrashSecurely -bool true

# show the ~/Library folder
echo "Setting to show ~/Library folder..."
chflags nohidden ~/Library

# stop pasting full names when copying an email address in mac osx mail
echo "Stopping pasting full names in mac osx mail..."
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

# only use utf-8 in Terminal.app
echo "Using only UTF-8 in Terminall.app..."
defaults write com.apple.terminal StringEncodings -array 4

# no more Time Machine prompting to use new hard drives as backup volumes
echo "No more Time Machine prompting to use new HDs as backup volumes..."
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

# disable local time machine backups
echo "To turn off local time machine backups...run:"
echo "  $hash tmutil &> /dev/null && sudo tmutil disablelocal"
# hash tmutil &> /dev/null && sudo tmutil disablelocal
echo "And enter with your password."
#echo "Complete."

# use plain text mode for new TextEdit documents
echo "Making TextEdit better..."
defaults write com.apple.TextEdit RichText -int 0
# open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# change where screen shots are saved to
echo "Screen Shots will be saved at ~/Pictures..."
defaults write com.apple.screencapture location ~/Pictures

# add playframework 1.2.5 into path
echo "Adding playframework 1.2.5 into path..."
if [ -d /Applications/play-1.2.5 ]; then
	echo '/Applications/play-1.2.5' >> /etc/paths
fi

# copy /etc/hosts
echo "Adding new /etc/hosts..."
cp -v ../toolbox/etc/hosts /etc/hosts

# install homebrew
if [ ! -f /usr/local/bin/brew ]; then
	echo "Installing homebrew..."
	ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
fi

# update homebrew
echo "Updating homebrew..."
brew update && brew upgrade

# install wget with homebrew
echo "Installing wget with homebrew..."
brew install wget

# install git with homebrew
echo "Installing git with homebrew..."
brew install git

# install bash-completion with homebrew
echo "Installing bash-completion with homebrew..."
brew install bash-completion

# install a python environment
echo "Installing a python complete environment (python/pip/distribute/virtualenv/mkvirtualenvwrapper)..."
if [ -f ./python-environment.sh ]; then
	bash ./python-environment.sh
fi

# install a java environment
echo "Installing a java environment (maven)..."
if [ -f ./java-environment.sh ]; then
	if [ -f /usr/bin/java ]; then
		bash ./java-environment.sh
	fi
fi

