#!/usr/bin/env bash

set -e

NIX="nix --extra-experimental-features flakes --extra-experimental-features nix-command"
CMD="${1:-switch}"
HOSTNAME="$(hostname -s)"

ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"

. "${ROOT}/scripts/bash-lib.sh"

setup_darwin_cmd_line_tools_license() {
        softwareupdate -i -a --agree-to-license
        sudo xcode-select --switch /Library/Developer/CommandLineTools

        _info "CommandLineTools installed."
	return 0
}

setup_darwin() {
	setup_darwin_cmd_line_tools_license

	NIX_ROOT="/run/current-system/sw"

	export PATH="${NIX_ROOT}/bin:$PATH"

	if ! which nix &>/dev/null; then
		_info "Nix not found, installing."

		curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --no-confirm

		_info "Nix installed."
	fi

	# Make sure we are connected to the Nix Daemon
	#
	# shellcheck source=/dev/null
	[ -e "${NIX_ROOT}/etc/profile.d/nix-daemon.sh" ] && . "${NIX_ROOT}/etc/profile.d/nix-daemon.sh"

	if [ ! -f /opt/homebrew/bin/brew ]; then
		_info "Homebrew not found, installing..."
		NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
                (echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"') >> ~/.zprofile
                eval "$(/opt/homebrew/bin/brew shellenv)"

		_info "Homebrew installed."
	fi
}

case "$(uname -s)" in
"Darwin")
    if [ "$(whoami)" == "root" ]; then
        _fatal "This script must be run as a normal user. Sudo password will be asked from you when required."
    fi

    setup_darwin
    ;;
*)
    _fatal "Invalid system"
    ;;
esac

_info "Done!"
