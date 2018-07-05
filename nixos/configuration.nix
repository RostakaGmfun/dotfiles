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

        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
        boot.kernelModules = [ "kvm-intel" ];

        hardware.nvidiaOptimus.disable = true;
        hardware.opengl = {
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
            extraPackages = [ config.boot.kernelPackages.nvidia_x11.out ];
            extraPackages32 = [ pkgs.pkgsi686Linux.linuxPackages.nvidia_x11.out ];
        };

        hardware.pulseaudio = {
            enable = true;
            support32Bit = true;
        };

        hardware.bumblebee = {
            enable = true;
            driver = "nvidia";
            connectDisplay = false;
        };

        services.xserver = {
            synaptics.enable = true;
            synaptics.twoFingerScroll = true;
            synaptics.vertEdgeScroll = true;
            dpi = 101;

            videoDrivers = [ "intel" ];
        };

        services.tlp.enable = true;

        services.openvpn.servers.cv = { config = '' config /root/rkurylo/vpn-gw2.ovpn ''; };

        environment.systemPackages = with pkgs; [
          kicad
        ];
        }
    ];
  }."${machine.name}";
in
{
  imports = [
    {
    nixpkgs.config.allowUnfree = true;

    networking.hostName = machine.name;
    networking.networkmanager.enable = true;

    virtualisation.virtualbox.host.enable = true;
    nixpkgs.config.virtualbox.enableExtensionPack = true;

    i18n = {
        consoleFont = "latarcyrheb-sun32";
        consoleKeyMap = "us";
        defaultLocale = "en_US.UTF-8";
    };

    time.timeZone = "Europe/Kiev";

    environment.systemPackages = with pkgs; [
        clearlooks-phenix
	dmenu
	emacs
	git
	godef
        google-chrome
        hicolor_icon_theme
        htop
	i3lock
	minicom
        networkmanagerapplet
	nox
	okular
        oxygen-icons5
        pavucontrol
        shared_mime_info
	slack
	unzip
        vim
	vlc
        wget
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
        windowManager.i3.enable = true;
    };


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
        extraGroups = ["wheel" "vboxusers" "plugdev"];
        home = "/home/gmfun";
        shell = pkgs.fish;
        isNormalUser = true;
        initialPassword = "password";
    };

    }
  ] ++ machine-config;
}
