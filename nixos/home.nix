{ config, pkgs, ... }:

{
  home.username = "lophophora";
  home.homeDirectory = "/home/lophophora";
  home.stateVersion = "25.11";

  # 安装必要包
  home.packages = with pkgs; [
    gnomeExtensions.paperwm
    gnomeExtensions.hide-universal-access
    lxgw-wenkai
  ];

  xdg.configFile."ibus/rime/ibus_rime.custom.yaml".text = ''
    patch:
     style/horizontal: true
     style/preedit_style: preview
  '';

  # GNOME 设置
  dconf.settings = {
    "org/gnome/shell" = {
      enabled-extensions = [
        "paperwm@paperwm.github.com"
        "hide-universal-access@akiirui.github.io"
      ];
    };
    "org/gnome/desktop/interface" = {
      font-name = "Adwaita Sans 10";
      text-scaling-factor = 1.35; # 缩放因子
    };
  };

  # Firefox 配置
  programs.firefox = {
    enable = true;
    profiles.default = {
      settings = {
        "layout.css.devPixelsPerPx" = "-1.0";
      };
    };
  };

  home.file.".local/share/applications/emacsclient-mail.desktop".text = "";
  home.file.".local/share/applications/emacs-mail.desktop".text = "";
}
