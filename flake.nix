{
  description = "Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }:
  let
    configuration = { pkgs, ... }: {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages = with pkgs; [
        aspell
        bat
        fzf
        gnupg
        pinentry_mac
        rcm
        ripgrep
      ];

      homebrew = {
        enable = true;

        taps = [
          "d12frosted/emacs-plus"
        ];

        brews = [
          "d12frosted/emacs-plus/emacs-plus@29"
        ];

        casks = [
          "font-fira-code-nerd-font"
          "keepingyouawake"
          "slack"
        ];

        caskArgs.no_quarantine = true;

        onActivation = {
          autoUpdate = true;
          upgrade = true;
          cleanup = "uninstall";
        };
      };

      security.pam.enableSudoTouchIdAuth = true;

      # https://daiderd.com/nix-darwin/manual/index.htm
      system = {
        keyboard = {
          enableKeyMapping = true;
          remapCapsLockToControl = true;
        };

        defaults = {
          trackpad = {
            TrackpadThreeFingerDrag = true;
          };

          finder = {
            ShowStatusBar = true;
            ShowPathbar = true;
            FXDefaultSearchScope = "SCcf";
          };

          screencapture = {
            location = "~/Pictures/screenshot";
          };

          NSGlobalDomain = {
            AppleInterfaceStyle = "Dark";
            AppleInterfaceStyleSwitchesAutomatically = false;

            "com.apple.mouse.tapBehavior" = 1;

            ApplePressAndHoldEnabled = false;
            KeyRepeat = 1;
            InitialKeyRepeat = 30;

            AppleFontSmoothing = 1;

            AppleShowAllFiles = true;
            AppleShowAllExtensions = true;
            NSDocumentSaveNewDocumentsToCloud = false;
          };
        };
      };

      # Auto upgrade nix package and the daemon service.
      services.nix-daemon.enable = true;
      # nix.package = pkgs.nix;

      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";

      # Create /etc/zshrc that loads the nix-darwin environment.
      programs.zsh.enable = true;  # default shell on catalina
      # programs.fish.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 5;

      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "aarch64-darwin";
    };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#macos
    darwinConfigurations."macos" = nix-darwin.lib.darwinSystem {
      modules = [ configuration ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."macos".pkgs;
  };
}
