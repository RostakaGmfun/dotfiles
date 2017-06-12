# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    grub.efiSupport = true;
    grub.device = "nodev";
  };

  hardware.nvidiaOptimus.disable = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;

  virtualisation.virtualbox.host.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Kiev";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
     wget vim powertop htop gnome3.dconf shared_mime_info networkmanagerapplet
     virtualbox
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.urxvtd.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us,ru,ua";
    xkbOptions = "grp:alt_shift_toggle, caps:swapescape";
    displayManager.lightdm.enable = true;
    displayManager.sessionCommands = ''
      xrdb -merge ~/.Xresources
      dbus-launch ${pkgs.networkmanagerapplet}/bin/nm-applet &
    '';
    windowManager.i3.enable = true;
  
    synaptics.enable = true;
    synaptics.twoFingerScroll = true;
    synaptics.vertEdgeScroll = true;
  };

  services.tlp.enable = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

  fonts = {
    fontconfig.dpi = 156;
    fontconfig.ultimate.enable = true;
    fonts = with pkgs; [
      corefonts
      inconsolata
    ];
  };

  programs.fish = {
    enable = true;
  };

  programs.light.enable = true;

  users.extraUsers.gmfun = {
    extraGroups = ["wheel" "vboxusers"];
    home = "/home/gmfun";
    shell = pkgs.fish;
    initialPassword = "password";
  };

}
