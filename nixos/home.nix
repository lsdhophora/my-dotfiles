{ config, pkgs, ... }:
let
  # 要删除的词列表
  deleteWords = [ ];
in
{
  home.username = "lophophora";
  home.homeDirectory = "/home/lophophora";
  home.stateVersion = "25.11";

  # 安装必要包
  home.packages = with pkgs; [
    gnomeExtensions.paperwm
    gnomeExtensions.hide-universal-access
  ];

  # Rime 配置
  xdg.configFile."ibus/rime" = {
    source = pkgs.fetchFromGitHub {
      owner = "iDvel";
      repo = "rime-ice";
      rev = "main";
      sha256 = "sha256-nPkwX5ruvCkQZ4yKNVuh72RmbLiEs/lkjb7mPQURVWw=";
    };
    recursive = true;
    force = true; # 允许覆盖现有文件
    # 在写入后保留 rime_ice.userdb 并删除指定词
    onChange = ''
      RIME_DIR="$HOME/.config/ibus/rime"
      # 备份 rime_ice.userdb 和词库文件
      if [ -f "$RIME_DIR/rime_ice.userdb" ]; then
        cp -v "$RIME_DIR/rime_ice.userdb" "$RIME_DIR/rime_ice.userdb.bak"
      fi
      # 备份词库文件以保留用户修改
      for file in "$RIME_DIR/cn_dicts"/*.dict.yaml "$RIME_DIR/en_dicts"/*.dict.yaml "$RIME_DIR/opencc/other.txt"; do
        if [ -f "$file" ]; then
          cp -v "$file" "$file.bak"
        fi
      done
      # 删除指定词
      ${pkgs.lib.concatMapStringsSep "\n" (word: ''
        for file in "$RIME_DIR/cn_dicts"/*.dict.yaml "$RIME_DIR/en_dicts"/*.dict.yaml "$RIME_DIR/opencc/other.txt"; do
          if [ -f "$file.bak" ]; then
            echo "Removing entries containing '${word}' from $file"
            ${pkgs.gnugrep}/bin/grep -vF "${word}" "$file.bak" > "$file"
          fi
        done
      '') deleteWords}
      # 恢复 rime_ice.userdb
      if [ -f "$RIME_DIR/rime_ice.userdb.bak" ]; then
        mv -v "$RIME_DIR/rime_ice.userdb.bak" "$RIME_DIR/rime_ice.userdb"
      fi
      # 清理备份的词库文件
      for file in "$RIME_DIR/cn_dicts"/*.dict.yaml.bak "$RIME_DIR/en_dicts"/*.dict.yaml.bak "$RIME_DIR/opencc/other.txt.bak"; do
        if [ -f "$file" ]; then
          rm -v "$file"
        fi
      done
      echo "Rime directory contents after operations:"
      ls -l "$RIME_DIR"
    '';
  };

  xdg.configFile."ibus/rime/rime_ice.custom.yaml".text = ''
    patch:
      editor/bindings:
        space: commit_raw_input
        Return: confirm
        Control+Return: commit_script_text
        Control+Shift+Return: commit_comment
        BackSpace: revert
        Delete: delete
        Control+BackSpace: back_syllable
        Control+Delete: delete_candidate
        Escape: cancel
  '';

  xdg.configFile."ibus/rime/default.custom.yaml".text = ''
    patch:
      ascii_composer/good_old_caps_lock: true
      ascii_composer/switch_key:
        Caps_Lock: clear
        Shift_L: noop
        Shift_R: noop
        Control_L: inline
        Control_R: noop
      key_binder/bindings:
        - { when: composing, accept: Control+p, send: Up }
        - { when: composing, accept: Control+n, send: Down }
        - { when: composing, accept: Control+f, send: Right }
        - { when: composing, accept: Control+b, send: Left }
        - { when: composing, accept: Control+a, send: Home }
        - { when: composing, accept: Control+e, send: End }
        - { when: composing, accept: Control+d, send: Delete }
        - { when: composing, accept: BackSpace, send: BackSpace }
        - { when: composing, accept: Control+k, send: delete_to_end }
  '';

  xdg.configFile."ibus/rime/ibus_rime.yaml".text = ''
    __build_info:
      rime_version: ${pkgs.librime.version}
      timestamps:
        ibus_rime: 0
        ibus_rime.custom: 0
      config_version: 1.0
      style:
        cursor_type: insert
        horizontal: true
        inline_preedit: true
        preedit_style: preview
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
      font-name = "Adwaita Sans 12";
      text-scaling-factor = 1.04; # 缩放因子
    };
  };

  # Firefox 配置
  programs.firefox = {
    enable = true;
    profiles.default = {
      settings = {
        "layout.css.devPixelsPerPx" = "1.1";
      };
    };
  };
}
