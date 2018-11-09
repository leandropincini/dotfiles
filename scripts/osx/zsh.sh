# install zsh and zsh-completions with homebrew
echo ">>> Installing zsh and zsh-completions with homebrew..."
brew install zsh zsh-completions

# install oh-my-zsh
if [ -f ./scripts/osx/oh-my-zsh/oh-my-zsh.sh ]; then
    bash ./scripts/osx/oh-my-zsh/oh-my-zsh.sh
fi
