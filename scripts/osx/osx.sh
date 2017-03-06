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
echo "Setting to save to disk (not to iCloud) by default..."
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

# finder: show path bar
echo "Setting finder to show path bar..."
defaults write com.apple.finder ShowPathBar -bool true

# when performing a search, search the current folder by default
echo "Setting finder to search the current folde by default..."
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# avoid creating .DS_Store files on network volumes,
# don't work with external usb drivers
echo "Avoiding creating .DS_Store files on network volumes..."
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# empty trash securely by defult
#echo "Setting to empty trash securely by default..."
#defaults write com.apple.finder EmptyTrashSecurely -bool true

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
echo "Turning off local time machine backups and snapshots..."
hash tmutil &> /dev/null && sudo tmutil disablelocal

# disable hibernation (speeds up entering sleep mode)
echo "Turning off hibernation..."
sudo pmset -a hibernatemode 0

# remove the sleep image file to save disk space
echo "Removing sleep image file..."
sudo rm /private/var/vm/sleepimage
#create a zero-byte file instead...
sudo touch /private/var/vm/sleepimage
# ... and make sure it can't be rewritten
sudo chflags uchg /private/var/vm/sleepimage

# disable the sudden motion sensor as it's not useful for SSDs
echo "Disabling motion sensor..."
sudo pmset -a sms 0

# use plain text mode for new TextEdit documents
echo "Making TextEdit better..."
defaults write com.apple.TextEdit RichText -int 0
# open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# require password immediataly after sleep or screen saver begins
echo "Require the password after sleep or screen saver begins..."
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# change where screen shots are saved to
echo "Screen Shots will be saved at ~/Pictures..."
defaults write com.apple.screencapture location ~/Pictures

# enable subpixel font rendering on non-Apple LCDs
echo "Better fonts for non-Appel LCDs..."
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# enable HiDPI display modes (requires restart)
sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true

# copy /etc/hosts
echo "Adding new /etc/hosts..."
sudo cp -v ./toolbox/etc/hosts /etc/hosts

# restart automatically if the computer freezes
echo "Restart if computer freezes..."
systemsetup -setrestartfreeze on

# enable tap to click
echo "Enabling tap to click with trackpad..."
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# enable tap to click for the login screen
echo "Enabling tap to click with trackpad for the login screen..."
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# enable three finger drag
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true

# increase sound quality for bluetooth headphones/headsets
echo "Better sound quality for bluetooth headphones/headsets..."
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

# enable auto-correct
echo "Auto correct ON..."
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool true

# install command line tools for xcode
if xcode-select --install 2>&1 | grep installed; then
        echo "Command line tools for xcode installed.";
else
        echo "Installing Command Line Tools for XCode...";
fi

# install homebrew
if [ ! -f /usr/local/bin/brew ]; then
	echo "Installing homebrew..."
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# update homebrew
echo "Updating homebrew..."
brew update && brew upgrade

# install git with homebrew
echo "Installing git with homebrew..."
brew install git

# install wget with homebrew
echo "Installing wget with homebrew..."
brew install wget

# install bash-completion with homebrew
echo "Installing bash-completion with homebrew..."
brew install bash-completion

# replace vim with macvim
#echo "Replacing vim with macvim (with homebrew)..."
#brew install macvim --env-std --with-override-system-vim

# install youtube-dl with homebrew
echo "Installing youtube-dl with homebrew..."
brew install youtube-dl

# install a python environment
if [ -f ./python-environment.sh ]; then
	echo "Installing a python environment (python/pip/distribute/virtualenv/mkvirtualenvwrapper)..."
	bash ./python-environment.sh
fi

# install a java environment
if [ -f ./java-environment.sh ]; then
	if [ -f /usr/bin/java ]; then
		echo "Installing a java environment (maven/jenkins)..."
		bash ./java-environment.sh
	fi
fi

# install emacs
#echo "Installing emacs with homebrew..."
#brew install emacs --cocoa --with-gnutls

# clearing
brew cleanup

