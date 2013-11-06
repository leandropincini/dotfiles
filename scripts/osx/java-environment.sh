# maven
echo "Installing maven with homebrew..."
brew install maven

# jenkins
echo "Installing jenkins with homebrew..."
brew install jenkins

# playframework
echo "Installing playframework environment..."
if [ -f ./scripts/osx/playframework.sh ]; then
	bash ./scripts/osx/playframework.sh
fi

