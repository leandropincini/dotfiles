# install oh-my-zsh custom plugins
echo ">>> Installing oh-my-zsh custom plugins..."
if [ ! -d ~/.oh-my-zsh/custom ]; then
    mkdir ~/.oh-my-zsh/custom
fi
if [ ! -d ~/.oh-my-zsh/custom/plugins ]; then
    mkdir ~/.oh-my-zsh/custom/plugins
fi
if [ ! -d ~/Projects ]; then
    mkdir ~/Projects
fi
if [ ! -d ~/Projects/oh-my-zsh-custom-plugins ]; then
    mkdir ~/Projects/oh-my-zsh-custom-plugins
fi
cd ~/Projects/oh-my-zsh-custom-plugins

# install zsh-syntax-highlighting
if [ -f /usr/bin/git ]; then
    echo ">>> >>> Installing zsh-syntax-highlighting plugin..."
    git clone --depth 1 https://github.com/zsh-users/zsh-syntax-highlighting.git
    ln -sf ~/Projects/oh-my-zsh-custom-plugins/zsh-syntax-highlighting ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting

    echo ">>> >>> Installing zsh-autosuggestions plugin..."
    git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions.git
    ln -sf ~/Projects/oh-my-zsh-custom-plugins/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
fi
