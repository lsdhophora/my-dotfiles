{ config, pkgs, ... }:

{
  home.username = "lophophora";
  home.homeDirectory = "/home/lophophora";

  home.stateVersion = "25.11";

  # 安装 yq
  home.packages = with pkgs; [
    yq-go # 确保 yq-go 已安装
  ];

  # 修补 rime_ice.schema.yaml 的 editor.bindings
  home.file.".config/ibus/rime" = {
    source =
      let
        rimeIce = pkgs.fetchFromGitHub {
          owner = "iDvel";
          repo = "rime-ice";
          rev = "main";
          sha256 = "sha256-iL5QMWxc93Avy/MuZ+MgT5GCbuGyv8s0rKD7U9eTGX8=";
        };
        # 使用 runCommand 修补 rime_ice.schema.yaml
        patchedRimeIce = pkgs.runCommand "rime-ice-patched" { nativeBuildInputs = [ pkgs.yq-go ]; } ''
          # 创建输出目录
          mkdir -p $out

          # 复制 rime-ice 文件到输出目录
          cp -r ${rimeIce}/* $out/

          # 使用 yq 删除 schema.editor.bindings 并替换顶层的 editor.bindings
          yq e 'del(.schema.editor.bindings) | del(.editor.bindings) | .editor.bindings = {
            "space": "commit_raw_input",
            "Return": "confirm",
            "Control+Return": "commit_script_text",
            "Control+Shift+Return": "commit_comment",
            "BackSpace": "revert",
            "Delete": "delete",
            "Control+BackSpace": "back_syllable",
            "Control+Delete": "delete_candidate",
            "Escape": "cancel"
          }' ${rimeIce}/rime_ice.schema.yaml > $out/rime_ice.schema.yaml.tmp

          # 移动临时文件到最终位置
          mv $out/rime_ice.schema.yaml.tmp $out/rime_ice.schema.yaml

          # 确保输出文件权限正确
          chmod 644 $out/rime_ice.schema.yaml
        '';
      in
      patchedRimeIce;
    recursive = true;
  };

  # 管理 ibus_rime.yaml
  home.file.".config/ibus/rime/ibus_rime.yaml" = {
    text = ''
      __build_info:
        rime_version: 1.14.0
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
    onChange = ''
      mkdir -p $HOME/.config/ibus/rime
      chown lophophora:users $HOME/.config/ibus/rime
    '';
  };
}
