# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, options, pkgs, system, ... }:

let
  nixos-hardware = builtins.fetchTarball {
    url = https://github.com/NixOS/nixos-hardware/archive/55f9eb6a73b2f932a9958810c5927feca02ce208.tar.gz;
    sha256 = "020i7jhvb66r9ddx463r2qcql25annfmhriy9higbkygl7g937zk";
  };
  overlays = import ./sources.nix;
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # Cachix config for anmonteiro.cachix.org
      ./cachix.nix

      # Include NixOS hardware quirks
      "${nixos-hardware}/lenovo/thinkpad/t480s"
      "${nixos-hardware}/common/pc/laptop/ssd"
    ];

  nixpkgs = {
    config = {
      allowUnfree = true;

      packageOverrides = _: import "${overlays}/boot.nix" {
        # I'm dumb but `config.nixpkgs.config` causes a stack overflow
        config = {
          allowUnfree = true;
        };
      };
    };
  };


  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;

    };
    kernelPackages = pkgs.linuxPackages_latest;
  };

  console = {
    earlySetup = true;
    font = "Lat2-Terminus16";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
  };

  # https://nixos.wiki/wiki/Bluetooth#Pairing_devices_from_the_command_line
  hardware.bluetooth.enable = true;

  networking = {
    hostName = "nixpad";
    networkmanager.enable = false;
    wireless.enable = true;
    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here. Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;

    interfaces = {
      enp0s31f6.useDHCP = true;

      wlp4s0.useDHCP = true;
    };
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    (import ./homies/common-packages.nix { inherit pkgs config; }) ++
    (import ./system-packages { inherit pkgs; });

  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      cat << EOF > $HOME/.zshrc
          . ${pkgs.callPackage ./homies/zshrc { inherit pkgs; }}/bin/zshrc
      EOF
    '';
    promptInit = "";
  };

  programs.light.enable = true;


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Clipboard
  services.clipmenu.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = [ "intel" ];
    layout = "us";
    xkbOptions = "ctrl:nocaps";

    # Higher is bigger
    # This sets Xft.dpi, which kitty reads from.
    # https://github.com/kovidgoyal/kitty/issues/109#issuecomment-320554447
    dpi = 75;

    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: with haskellPackages; [
          xmonad
          xmonad-contrib
          xmonad-extras
          xmobar
        ];
        config = builtins.readFile ./xmonad/xmonad.hs;
      };
    };

    displayManager = {
      sessionCommands = with pkgs; lib.mkAfter
        ''
          xmodmap $HOME/.Xmodmap
        '';

      defaultSession = "none+xmonad";
    };
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "Noto Serif" ];
        sansSerif = [ "Noto Sans" ];
        monospace = [ "Fira Code" ];
      };
    };
  };

  services.actkbd = {
    enable = true;
    bindings = [
      # Brightness
      {
        keys = [ 224 ];
        events = [ "key" "rep" ];
        command = "/run/current-system/sw/bin/light -U 5";
      }
      {
        keys = [ 225 ];
        events = [ "key" "rep" ];
        command = "/run/current-system/sw/bin/light -A 5";
      }
    ];
  };


  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.anmonteiro = {
    isNormalUser = true;
    home = "/home/anmonteiro";
    description = "Antonio Monteiro";
    extraGroups = [ "wheel" "audio" "video" ]; # Enable ‘sudo’ for the user.
    hashedPassword = "$6$FsHUqlBu4PPnYyA$e3uGB9b8gNIAE/D2II8o4pcdUFrSXhXYxtfVkrSZoE4KY.j1pZbEmXFn73/S8GWZPo7dNgCYobZWsbHMhsFdv1";
    shell = pkgs.zsh;
  };
  users.mutableUsers = false;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  # Only use 1GB of logs max for journald
  services.journald.extraConfig = "SystemMaxUse=1G";

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 3d";
    };

    trustedUsers = [ "root" "@wheel" ];
  };
}

# TODOs:
# - screen lock
# - hibernation?
# - f.lux lighting thing
# - fingerprint sensor
