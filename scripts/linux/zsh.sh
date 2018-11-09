# install zsh and zsh-completions with dnf
echo ">>> Installing zsh and with dnf..."
sudo dnf install zsh -y

# install oh-my-zsh
if [ -f ./scripts/linux/oh-my-zsh/oh-my-zsh.sh ]; then
    bash ./scripts/linux/oh-my-zsh/oh-my-zsh.sh
fi

#chsh -s $(which zsh)
