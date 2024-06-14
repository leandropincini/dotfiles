# Mac OSX Setup

## Computer's name

```bash
sudo scutil --set ComputerName "tinydancer" &&
sudo scutil --set HostName "tinydancer" &&
sudo scutil --set LocalHostName "tinydancer" &&
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "tinydancer"
```

Add the following in the /etc/hosts file:

```bash
127.0.0.1	localhost	tinydancer
255.255.255.255	broadcasthost
::1             localhost
fe80::1%lo0	localhost
```

## Trackpad

### Enable tap to click

```bash
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true &&
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1 &&
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
```

### Enable tap to click for the login screen

```bash
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1 &&
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
```

### Enable three finger drag

```bash
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true &&
defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true
```

## Homebrew

Install [Homebrew](https://brew.sh/):

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Update

```bash
brew update && brew upgrade
```

## asdf

Install [asdf](https://asdf-vm.com/) with brew:

```bash
brew install asdf
```

### Java

Install asdf's [java plugin](https://github.com/halcyon/asdf-java):

```bash
asdf plugin-add java https://github.com/halcyon/asdf-java.git &&
adsf plugin update --all
```

Take a look into the jdk releases list: https://www.java.com/releases/matrix/

```bash
asdf install java openjdk-22.0.1 &&
asdf global java openjdk-22.0.1
```

Add the following in the `~/.zshrc`:

```bash
# JAVA_HOME with asdf
. ~/.asdf/plugins/java/set-java-home.zsh
```

Add the following in the `~/.asdfrc`:

```bash
java_macos_integration_enable=yes
``` 

## Programs

```bash
brew install rcm golang gnucash keepingyouawake ripgrep bat
```

### Go lang

```bash
go install github.com/swaggo/swag/cmd/swag@latest 
```

Add the following in the `~/.zshrc`:

```bash
# Go lang
export PATH=$PATH:$(go env GOPATH)/bin
export PATH=$PATH:$(go env GOROOT)/bin
```

## Dotfiles
```bash
cd ~
git clone git@github.com:leandropincini/dotfiles.git .dotfiles --depth=1
cd .dotfiles
./install-macos.sh
```

