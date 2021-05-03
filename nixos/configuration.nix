{ config, lib, pkgs, ... }:
let
  machine = import ./machine.nix;
  machine-config = {
    zen = [
    {
	imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];
        fileSystems."/" =
            { device = "/dev/disk/by-uuid/fc60becf-304e-46d3-983c-80ada2068ab9";
            fsType = "ext4";
            };

        fileSystems."/data" =
            {
            device = "/dev/disk/by-label/data";
            fsType = "ext4";
            };

        nix.maxJobs = lib.mkDefault 4;

        # Use the systemd-boot EFI boot loader.
        boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
            grub.efiSupport = true;
            grub.device = "nodev";
        };

        boot.kernelPackages = pkgs.linuxPackages_5_4;

        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.blacklistedKernelModules = ["qcserial"];

        hardware.nvidiaOptimus.disable = false;
        hardware.bumblebee.enable = true;
        hardware.opengl = {
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
        };

        hardware.pulseaudio = {
            enable = true;
            support32Bit = true;
        };

        services.xserver = {
            libinput = {
              enable = true;
              disableWhileTyping = true;
            };
            dpi = 96;
            videoDrivers = [ "intel" ];
        };

        services.tlp.enable = true;
        }
    ];
  }."${machine.name}";
in
{
  imports = [
    {

    nixpkgs.config = {
      allowUnfree = true;
    };

    networking.hostName = machine.name;
    networking.networkmanager.enable = true;
    networking.networkmanager.unmanaged = [ "wlp0s20f0u1" ];
    networking.firewall.enable = false;

    #virtualisation.virtualbox.host.enable = true;
    #nixpkgs.config.virtualbox.enableExtensionPack = true;

    virtualisation.docker.enable = true;


    nix.nixPath = [
      "nixos-config=/home/gmfun/dotfiles/nixos/configuration.nix"
      "/home/gmfun/dotfiles/nix-channels"
    ];

    console = {
        font = "latarcyrheb-sun32";
        keyMap = "us";
    };

    i18n.defaultLocale = "en_US.UTF-8";

    time.timeZone = "Europe/Kiev";

    environment.systemPackages = with pkgs; [
      ag
      bc
      binutils
      bridge-utils
      cdecl
      colordiff
      clearlooks-phenix
      dmenu
      emacs26
      ffmpeg-full
      file
      findutils
      fzf
      gcc
      gcc-arm-embedded
      gdb
      gimp
      git
      global
      gnome3.gnome-screenshot
      gnugrep
      google-chrome
      gthumb
      hicolor_icon_theme
      htop
      i3lock
      i3status
      iptables
      iw
      libreoffice
      minicom
      nix-prefetch-scripts
      networkmanagerapplet
      nox
      okular
      openocd
      oxygen-icons5
      patchelf
      pciutils
      pavucontrol
      python36Packages.virtualenv
      qemu
      rustup
      shared_mime_info
      tcpdump
      telnet
      transmission-gtk
      tree
      unrar
      unzip
      usbutils
      vim
      vlc
      wget
      wireshark
      xfce.thunar
      xorg.xkill
      zip
    ];


    environment.variables = { XDG_CURRENT_DESKTOP = "kde"; };

    # Enable the OpenSSH daemon.
    services.openssh.enable = true;

    # Enable the X11 windowing system.
    programs.fish.enable = true;
    services.xserver = {
        enable = true;
        layout = "us,ru,ua";
        xkbOptions = "grp:alt_shift_toggle, caps:swapescape";
        displayManager.lightdm.enable = true;
        displayManager.sessionCommands = ''
        xrdb -merge ~/.Xresources
        dbus-launch ${pkgs.networkmanagerapplet}/bin/nm-applet &
        '';
        displayManager.defaultSession = "none+i3";
        windowManager.i3.enable = true;
    };
    programs.gnome-terminal.enable = true;

    # The NixOS release to be compatible with for stateful data such as databases.
    system.stateVersion = "18.03";

    fonts = {
        fonts = with pkgs; [
        corefonts
        inconsolata
        ];
    };

    users.extraGroups.plugdev = { };

    users.extraUsers.gmfun = {
        extraGroups = ["wheel" "vboxusers" "plugdev" "docker"];
        home = "/home/gmfun";
        shell = pkgs.fish;
        isNormalUser = true;
        initialPassword = "password";
    };

    services.printing.enable = true;
    services.avahi.enable = true;
    services.avahi.nssmdns = true;

    }
  ] ++ machine-config;
}
