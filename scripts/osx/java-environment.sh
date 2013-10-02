# maven
echo "Installing maven with homebrew..."
brew install maven

# jenkins
echo "Installing jenkins with homebrew..."
brew install jenkins

# playframework
echo "Installing playframework environment..."
if [ -f ./playframework.sh ]; then
	bash playframework.sh
fi

