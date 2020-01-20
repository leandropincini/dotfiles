ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# update homebrew
echo "Updating homebrew..."
brew update && brew upgrade

# install git with homebrew
echo "Installing git with homebrew..."
brew install git

# install wget with homebrew
echo "Installing wget with homebrew..."
brew install wget

# install p7zip with homebrew
echo "Installing p7zip with homebrew..."
brew install p7zip

# install bash-completion with homebrew
#echo "Installing bash-completion with homebrew..."
#brew install bash-completion

# replace vim with macvim
#echo "Replacing vim with macvim (with homebrew)..."
#brew install macvim --env-std --with-override-system-vim

# install gnutls with homebrew
echo "Installing gnutls with homebrew..."
brew install gnutls

# install emacs with homebrew
#echo "Installing emacs and actvating emacs service with homebrew..."
#brew install emacs --with-cocoa
#brew services start emacs

# install youtube-dl with homebrew
echo "Installing youtube-dl with homebrew..."
brew install youtube-dl

# install terminal-notifier with homebrew
echo "Installing terminal-notifier with homebrew..."
brew install terminal-notifier

# install the_silver_searcher with homebrew
#echo "Installing the_silver_searcher with homebrew..."
#brew install the_silver_searcher

# install ripgrep with homebrew
echo "Installing ripgrep with homebrew..."
brew install ripgrep

if [ -f ./brew-cask.sh ]; then
    bash ./brew-cask.sh
fi
