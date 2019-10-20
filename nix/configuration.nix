# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, system, ... }:

let nixos-hardware = builtins.fetchTarball {
  url = "https://github.com/NixOS/nixos-hardware/archive/master/master.tar.gz";
};

in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # Include NixOS hardware quirks
      "${nixos-hardware}/lenovo/thinkpad"
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.blacklistedKernelModules = [ "nouveau" ];

  networking.hostName = "nixpad"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; 
    (import ./homies/common-packages.nix pkgs) ++ 
    [
      dmenu
      xlibs.xmodmap

      brave
    ];

  nixpkgs.config.allowUnfree = true;

  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      cat << EOF > $HOME/.zshrc
          . ${pkgs.callPackage ./homies/zshrc { inherit pkgs; }}/bin/zshrc
      EOF
    '';
    promptInit = "";
  };

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
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Clipboard
  services.clipmenu.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    
    windowManager = {
      xmonad = {
        enable = true;
	enableContribAndExtras = true;
	extraPackages = haskellPackages: [
	  haskellPackages.xmonad-contrib
	  haskellPackages.xmonad-extras
	  haskellPackages.xmonad
	];
      };
      default = "xmonad";
    };

    displayManager = {
      sessionCommands = with pkgs; lib.mkAfter 
        ''
	xmodmap $HOME/.Xmodmap
	'';
    };

    desktopManager.default = "none";
  };
  # services.xserver.xkbOptions = "eurosign:e";

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
     extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
     hashedPassword = "$6$FsHUqlBu4PPnYyA$e3uGB9b8gNIAE/D2II8o4pcdUFrSXhXYxtfVkrSZoE4KY.j1pZbEmXFn73/S8GWZPo7dNgCYobZWsbHMhsFdv1";
     shell = pkgs.zsh;
   };
   users.mutableUsers = false;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
