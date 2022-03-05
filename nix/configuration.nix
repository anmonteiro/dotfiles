# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, options, pkgs, system, lib, ... }:

{

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
    (callPackage ./homies/common-packages.nix { inherit config; }) ++
    (callPackage ./system-packages { });

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
  services.openssh.enable = true;

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
    layout = "us,pt";
    xkbOptions = "ctrl:nocaps, grp:alt_space_toggle";

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    users = {
      root.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKhezaZPIx4/UkbTm27qaTqOwlfmVNovc33p6L1p+dHG anmonteiro@gmail.com"
      ];
      anmonteiro = {
        isNormalUser = true;
        home = "/home/anmonteiro";
        description = "Antonio Monteiro";
        extraGroups = [ "wheel" "audio" "video" "wireshark" ]; # Enable ‘sudo’ for the user.
        hashedPassword = "$6$FsHUqlBu4PPnYyA$e3uGB9b8gNIAE/D2II8o4pcdUFrSXhXYxtfVkrSZoE4KY.j1pZbEmXFn73/S8GWZPo7dNgCYobZWsbHMhsFdv1";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKhezaZPIx4/UkbTm27qaTqOwlfmVNovc33p6L1p+dHG anmonteiro@gmail.com"
        ];
        shell = pkgs.zsh;
      };
    };
    mutableUsers = false;
  };

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

    settings = {
      trusted-users = [ "root" "@wheel" ];
    };
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}

# TODOs:
# - screen lock
# - hibernation?
# - f.lux lighting thing
# - fingerprint sensor
# - keyboard layout indicator in status bar
