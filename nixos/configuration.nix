{ config, lib, pkgs, ... }:
let
  machine = import ./machine.nix;
  machine-config = {
    thinkpad = [
    {
        imports =
            [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
            ];

        boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.extraModulePackages = [ ];

        fileSystems."/" =
            { device = "/dev/disk/by-uuid/0d9fbb1c-e4ab-49fb-b5d3-aa1a70cd6d0c";
            fsType = "ext4";
            };

        fileSystems."/boot" =
            { device = "/dev/disk/by-uuid/F752-B063";
            fsType = "vfat";
            };

        swapDevices = [ ];

        nix.maxJobs = lib.mkDefault 8;
        powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

        # Use the systemd-boot EFI boot loader.
        boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
            grub.efiSupport = true;
            grub.device = "nodev";
        };

        hardware.nvidiaOptimus.disable = true;

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

        fonts.fontconfig.dpi = 156;

        services.tlp.enable = true;
        programs.light.enable = true;
    }
    ];
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
          light
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
            name = "rtags-2.18";
            src = pkgs.fetchgit {
              # This commit fixes protocol version mismatch between elisp and C++ code (124 vs 125)
              rev = "163c81ea636c2aaca78e76df174bfd5679015bd7";
              fetchSubmodules = true;
              url = "https://github.com/andersbakken/rtags.git";
              sha256 = "0g4d4cv8fp55f5k6qlq7kvmwkmrc34fg6dq5w4mj67zmga21mqzp";
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

    #virtualisation.virtualbox.host.enable = true;
    #nixpkgs.config.virtualbox.enableExtensionPack = true;

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
        libcxx
        clearlooks-phenix
        dmenu
        dolphin
        emacs
        git
        global
        godef
        google-chrome
        hicolor_icon_theme
        htop
        i3lock
        i3status
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
        desktopManager.plasma5.enable = true;
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
      description = "rtags daemon socket1";
      wantedBy = [ "sockets.target" ];
      listenStreams = [ "/tmp/rdm.socket" ];
    };

    systemd.services.rdm  = {
      description = "rtags daemon for emacs-rtags";
      requires = [ "rdm.socket" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.rtags}/bin/rdm -v --log-flush --socket-file /tmp/rdm.socket --isystem ${pkgs.libcxx}/include/c++/v1";
        ExecStartPost = ''/bin/sh -c "echo +19 > /proc/$MAINPID/autogroup"'';
        Nice = 19;
        CPUSchedulingPolicy = "idle";
      };
    };

    services.printing.enable = true;
    services.avahi.enable = true;
    services.avahi.nssmdns = true;

    }
  ] ++ machine-config;
}
