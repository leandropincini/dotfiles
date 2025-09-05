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

### Setting finder to show all filename extensions
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
defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true
```

### Enable Retina-aware font smoothing:
```bash
defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO
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
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" &&
eval "$(/opt/homebrew/bin/brew shellenv)" >> ~/.zprofile
```

### Update
```bash
brew update && brew upgrade
```

## Programs
```bash
brew install gnucash obs rcm visual-studio-code keepingyouawake git ripgrep bat fzf font-fira-code-nerd-font font-iosevka-term-nerd-font font-jetbrains-mono font-jetbrains-mono-nerd-font alacritty ghostty starship
```

### Alacritty

Better font display
```bash
defaults write org.alacritty AppleFontSmoothing -int 0
```

### Emacs
```bash
brew tap d12frosted/emacs-plus &&
brew install aspell d12frosted/emacs-plus/emacs-plus@30
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
cat ~/.ssh/id_ed25519.pub | pbcopy
```

#### gpg key - execute it after the [Dotfiles](#dotfiles) session.

Optionally, install a graphical prompt with save in keychain option
```bash
brew install pinentry-mac &&
echo "pinentry-program $(which pinentry-mac)" >> ~/.gnupg/gpg-agent.conf &&
killall gpg-agent
```

Generate the GPG Key

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

## Dotfiles
```bash
cd ~ &&
mkdir ~/.cache/zsh &&
git clone git@github.com:leandropincini/dotfiles.git .dotfiles --depth=1 &&
cd .dotfiles &&
./install-macos.sh &&
mkdir ~/projects &&
ln -s ~/.dotfiles ~/projects/dotfiles
```

## asdf

Install [asdf](https://asdf-vm.com/) with brew:
```bash
brew install asdf
```

### Java

Install asdf's [java plugin](https://github.com/halcyon/asdf-java):
```bash
asdf plugin add java https://github.com/halcyon/asdf-java.git
```

Take a look into the jdk releases list: https://www.java.com/releases/matrix/
```bash
asdf install java adoptopenjdk-24.0.1+9 &&
asdf set -u java adoptopenjdk-24.0.1+9
```

Add the following in the `~/.asdfrc`:
```bash
java_macos_integration_enable=yes
```

### Go lang
```bash
asdf plugin add golang https://github.com/asdf-community/asdf-golang.git &&
asdf install golang 1.24.4 &&
asdf set -u golang 1.24.4 &&
brew install protobuf grpcurl grpcui &&
cd ~ &&
go-setup-dev-env
# libs
# go install github.com/swaggo/swag/cmd/swag@latest &&
# go install github.com/google/wire/cmd/wire@latest &&
asdf reshim golang
```

Open your vscode and type Cmd + Shift + P
```bash
>go install/update tools
```

Select and install everything.

### Setup ~/.zshrc
```bash
# asdf
if [ -f "/opt/homebrew/opt/asdf/bin/asdf" ]; then
    # Add asdf and shims to PATH (before other PATH modifications)
    export ASDF_DIR="/opt/homebrew/opt/asdf"
    export PATH="$HOME/.asdf/shims:$ASDF_DIR/bin:$PATH"

    # Initialize asdf
    eval "$(asdf exec env)"

    # Add completions to fpath
    fpath+=("$(brew --prefix asdf)/share/zsh/site-functions")
    autoload -Uz compinit && compinit

    # JAVA_HOME with asdf
    if [ -f "$HOME/.asdf/plugins/java/set-java-home.zsh" ]; then
        . "$HOME/.asdf/plugins/java/set-java-home.zsh"
    fi

    # GO lang with asdf
    export ASDF_GOLANG_MOD_VERSION_ENABLED=true
    if [ -f "$HOME/.asdf/plugins/golang/set-env.zsh" ]; then
        . "$HOME/.asdf/plugins/golang/set-env.zsh"
    fi
fi
```
