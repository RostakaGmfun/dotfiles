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

        boot.kernelPackages = pkgs.linuxPackages_4_9;

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
            libinput = {
              enable = true;
              disableWhileTyping = true;
            };
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

    nixpkgs.config = {
      allowUnfree = true;

      # Override rtags package with a newer version compatible with rtags
      packageOverrides = pkgs: rec {
        rtags = pkgs.rtags.overrideDerivation (oldAttrs: {
            name = "rtags-2.10";
            src = pkgs.fetchgit {
              rev = "refs/tags/v2.10";
              fetchSubmodules = true;
              url = "https://github.com/andersbakken/rtags.git";
              sha256 = "0rv5hz4cfc1adpxvp4j4227nfc0p0yrjdc6l9i32jj11p69a5401";
              # unicode file names lead to different checksums on HFS+ vs. other
              # filesystems because of unicode normalisation
              postFetch = ''
              rm $out/src/rct/tests/testfile_*.txt
              '';
            };
        });
      };
    };

    networking.hostName = machine.name;
    networking.networkmanager.enable = true;

    virtualisation.virtualbox.host.enable = true;
    nixpkgs.config.virtualbox.enableExtensionPack = true;

    virtualisation.docker.enable = true;


    nix.nixPath = [
      "nixos-config=/home/gmfun/dotfiles/nixos/configuration.nix"
      "/home/gmfun/dotfiles/nix-channels"
    ];

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
        global
        godef
        google-chrome
        hicolor_icon_theme
        htop
        i3lock
        minicom
        nix-prefetch-scripts
        networkmanagerapplet
        nox
        okular
        oxygen-icons5
        pavucontrol
        python36Packages.virtualenv
        rtags
        shared_mime_info
        slack
        unzip
        vagrant
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
        extraGroups = ["wheel" "vboxusers" "plugdev" "docker"];
        home = "/home/gmfun";
        shell = pkgs.fish;
        isNormalUser = true;
        initialPassword = "password";
    };

    systemd.sockets.rdm  = {
      description = "rtags daemon socket";
      listenStreams = [ "%t/rdm.socket" ];
      wantedBy = [ "default.target" ];
    };

    systemd.services.rdm  = {
      description = "rtags daemon for emacs-rtags";
      wantedBy = [ "default.target" ];
      requires = [ "rdm.socket" ];
      serviceConfig = {
        Type = "simple";
        User = "gmfun";
        ExecStart = "${pkgs.rtags}/bin/rdm";
        ExecStop = "";
        Nice = 19;
        CPUSchedulingPolicy="idle";
      };
    };

    }
  ] ++ machine-config;
}
