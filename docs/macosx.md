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
## Finder

### Setting finder to show all filename extensios
```bash
defaults write com.apple.finder AppleShowAllExtensions -bool true
```

### Setting finder to show status bar
```bash
defaults write com.apple.finder ShowStatusBar -bool true
```

### Seeting finder to show path bar
```bash
defaults write com.apple.finder ShowPathBar -bool true
```

### Setting finder to search the current folder by default
```bash
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
```

### Show Library folder
```bash
chflags nohidden ~/Library
```

## Time Machine

### No more Time Machine prompting to use new HDs as backup volumes
```bash
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
```

### Turn off local Time Machine backpus
```bash
hash tmutil &> /dev/null && sudo tmutil disablelocal
```

### Require password immediataly after sleep or screen saver begins
```bash
defaults write com.apple.screensaver askForPassword -int 1 &&
defaults write com.apple.screensaver askForPasswordDelay -int 0
```

## Display

### Enable subpixel font rendering on non-Apple LCDs
```bash
defaults write NSGlobalDomain AppleFontSmoothing -int 2
```

### Enable HiDPI display modes (requires restart)
```bash
sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true
```

## Sound

### Increase sound quality for bluetooth headphones/headsets
```bash
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40
```

## Utils

### Do not create .DS_Store files on network volumes
```bash
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
```

### Using only UTF-8 on Terminal.app
```bash
defaults write com.apple.terminal StringEncodings -array 4
```

### Save screen shots at ~/Pictures
```bash
defaults write com.apple.screencapture location ~/Pictures
```

### Enable Auto Correct
```bash
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool true
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
asdf plugin update --all
```

Take a look into the jdk releases list: https://www.java.com/releases/matrix/
```bash
asdf install java openjdk-22.0.2 &&
asdf global java openjdk-22.0.2
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
brew install rcm visual-studio-code gnucash keepingyouawake ripgrep bat fzf
```

### Go lang
```bash
asdf plugin add golang https://github.com/asdf-community/asdf-golang.git &&
asdf install golang 1.23.1 &&
asdf global golang 1.23.1 &&
brew install protobuf &&
cd ~ &&
go install github.com/swaggo/swag/cmd/swag@latest &&
go install github.com/google/wire/cmd/wire@latest &&
go install golang.org/x/perf/cmd/benchstat@latest &&
go install google.golang.org/protobuf/cmd/protoc-gen-go &&
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc &&
asdf reshim golang
```

Open your vscode and type Cmd + Shift + P
```bash
>go install/update tools
```

Select and install everything.

### Emacs

```bash
brew tap d12frosted/emacs-plus &&
brew install aspell font-fira-code-nerd-font d12frosted/emacs-plus/emacs-plus@30
```

### Github setup
#### ssh key
Create a new ssh-key
```bash
ssh-keygen -t ed25519 -C "leandropincini@gmail.com"
```

Enable your ssh-agent
```bash
eval "$(ssh-agent -s)"
```

Setup your `~/.ssh/config`
```
Host github.com
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519
```

Register your ssh key into apple-key-chain
```bash
ssh-add --apple-use-keychain ~/.ssh/id_ed25519
```

Paste the result into github session
```bash
cat ~/.ssh/id_ed25519.pub
```

#### gpg key
```bash
brew install gnupg &&
gpg --full-generate-key
```

Export the pub key and use it (change it to your id)
```bash
gpg --list-secret-keys --keyid-format=long &&
gpg --armor --export 3AA5C34371567BD2 &&
git config --global user.signingkey 3AA5C34371567BD2 &&
git config --global commit.gpgsign true &&
if [ -r ~/.zshrc ]; then echo -e '\nexport GPG_TTY=$(tty)' >> ~/.zshrc; \
  else echo -e '\nexport GPG_TTY=$(tty)' >> ~/.zprofile; fi
```

Optionally, install a graphical prompt with save in keychain option
```bash
brew install pinentry-mac &&
echo "pinentry-program $(which pinentry-mac)" >> ~/.gnupg/gpg-agent.conf &&
killall gpg-agent
```

## Dotfiles
```bash
cd ~ &&
git clone git@github.com:leandropincini/dotfiles.git .dotfiles --depth=1 &&
cd .dotfiles &&
./install-macos.sh
```
