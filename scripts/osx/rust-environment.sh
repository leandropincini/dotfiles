echo "Installing rust..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

echo "Installing bat with cargo..."
cargo install bat

echo "Installing ripgrep with cargo..."
cargo install ripgrep
