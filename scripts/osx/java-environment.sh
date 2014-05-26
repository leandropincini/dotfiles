# maven
echo "Installing maven with homebrew..."
brew install maven

# gradle
echo "Installing gradle with homebrew..."
brew install gradle

# jenkins
echo "Installing jenkins with homebrew..."
brew install jenkins

# playframework
echo "Installing playframework environment..."
if [ -f ./playframework.sh ]; then
	bash ./playframework.sh
fi

