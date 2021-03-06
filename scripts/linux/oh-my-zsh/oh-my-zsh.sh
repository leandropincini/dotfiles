# install oh-my-zsh
echo ">>> Installing oh-my-zsh..."

if [ -d ~/.oh-my-zsh ]; then
    rm -rf ~/.oh-my-zsh
fi

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install oh-my-zsh dracula-theme
echo ">>> Installing oh-my-zsh dracula-theme..."
if [ -f /usr/bin/git ]; then
    if [ ! -d ~/.oh-my-zsh/themes ]; then
        mkdir ~/.oh-my-zsh/themes
    fi
    if [ ! -d ~/Projects ]; then
        mkdir ~/Projects
    fi
    if [ ! -d ~/Projects/dracula-theme ]; then
        mkdir ~/Projects/dracula-theme
    fi

    cd ~/Projects/dracula-theme

    if [ -f /usr/bin/git ]; then
        git clone --depth 1 https://github.com/dracula/zsh.git
        ln -s ~/Projects/dracula-theme/zsh/dracula.zsh-theme ~/.oh-my-zsh/themes/dracula.zsh-theme
    fi
fi

# install oh-my-zsh custom plugins
if [ -f ./scripts/linux/oh-my-zsh/oh-my-zsh-plugins.sh ]; then
    bash ./scripts/linux/oh-my-zsh/oh-my-zsh-plugins.sh
fi
