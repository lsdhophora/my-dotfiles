# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, pkgs, lib, inputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot = {
    # 启用 Plymouth，使用默认主题
    plymouth.enable = true;

    # 静默启动参数
    consoleLogLevel = 3;
    initrd.verbose = false;
    kernelParams = [ "quiet" "splash" "udev.log_priority=3" "rd.systemd.show_status=auto" ];

    # 使用 systemd-boot 作为 EFI 启动加载器
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # 内核模块和最新内核
    kernelModules = [ "amd_pstate" ];
    kernelPackages = pkgs.linuxPackages_latest;
  };

  networking.hostName = "flowerpot"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Asia/Shanghai";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.excludePackages = [ pkgs.xterm ];

  # Enable the GNOME Desktop Environment.
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
  environment.gnome.excludePackages = (with pkgs; [
    atomix # puzzle game
    cheese # webcam tool
    epiphany # web browser
    geary # email reader
    gedit # text editor
    gnome-characters
    gnome-photos
    gnome-tour
    hitori # sudoku game
    iagno # go game
    tali # poker game
    totem # video player
    yelp
  ]);


  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lophophora = {
     isNormalUser = true;
     hashedPassword = "$y$j9T$ywlcAEMJIDVX/1G5Pm5bi1$YSb/zJEgyNykoFHYt0F8b5DZ8mK9GZE.QlQzMfOfUO3";
     extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
     shell = pkgs.fish;
     packages = with pkgs; [
       tree
       fastfetch
       imagemagick
       emacs-pgtk
       gnome-themes-extra
       gnome-tweaks
     ];
   };

  programs.fish.enable = true;

  programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
     nano
     git
     wget
     texlive.combined.scheme-full
     texlab
     inputs.agenix.packages."${system}".default
   ];

  documentation.nixos.enable = false;

   # dae service

  age.secrets.daeConfig = {
    file = ./secrets/config.dae.age;
    path = "/run/secrets/dae/config.dae"; # Where the decrypted file will be placed
    mode = "600"; # Permissions for the decrypted file
    owner = "root"; # Adjust to the user running dae, if needed
    group = "root";
  };
  
   services.dae = {
     enable = true;
     configFile = config.age.secrets.daeConfig.path;
  };
   
  # font config and input method
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      noto-fonts-cjk-sans
      noto-fonts-emoji
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        sansSerif = [ "Noto Sans CJK SC" ];
        serif = [ "Noto Serif CJK SC" ];
        monospace = [ "Noto Sans Mono CJK SC" ];
        emoji = [ "Noto Color Emoji" ]; 
      };
    };
  };

  i18n.inputMethod = {
    enable = true;             
    type = "ibus";    
    ibus.engines = with pkgs.ibus-engines; [
      rime
    ];
  }; 
   
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users = [ "root" "lophophora" ];

  programs.git = {
    enable = true;
    config = {
      user = {
        name = "lsdhophora";
        email = "lsdphophora@proton.me";
       };
     init.defaultBranch = "main"; 
    };
  };
 
 age.secrets.nix-access-tokens-github = {
    file = ./secrets/nix-access-tokens-github.age;
    path = "/run/agenix/nix-access-tokens-github";
    owner = "root";
    group = "root";
    mode = "600";
  };

  age.identityPaths = [ "/home/lophophora/.ssh/id_ed25519" ];
  
  nix.extraOptions = ''
    !include ${config.age.secrets.nix-access-tokens-github.path}
  '';

  system.stateVersion = "25.05";    
}

