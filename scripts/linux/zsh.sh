# install zsh and zsh-completions with yum
echo ">>> Installing zsh and with yum..."
sudo yum install zsh -y

# install oh-my-zsh
if [ -f ./scripts/linux/oh-my-zsh/oh-my-zsh.sh ]; then
    bash ./scripts/linux/oh-my-zsh/oh-my-zsh.sh
fi

chsh -s $(which zsh)
