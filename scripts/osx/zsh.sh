# install zsh and zsh-completions with homebrew
echo "Installing zsh and zsh-completions with homebrew..."
brew install zsh zsh-completions

# install oh my zsh
echo "Installing oh my zsh..."
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install oh my zsh dracula theme
echo "Installing oh my zsh dracula-theme..."
if ![ -d ~/Projects ]; then
    mkdir ~/Projects
fi
if ![ -d ~/Projects/dracula-theme ]; then
    mkdir ~/Projects/dracula-theme
fi
cd ~/Projects/dracula-theme
git clone https://github.com/dracula/zsh.git
ln -s ~/Projects/dracula-theme/zsh/dracula.zsh-theme ~/.oh-my-zsh/themes/dracula.zsh-theme
