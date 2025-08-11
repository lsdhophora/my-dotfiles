{ config, pkgs, ... }:

{
  home.username = "lophophora";
  home.homeDirectory = "/home/lophophora";
  home.stateVersion = "25.11";

  # Clone rime-ice from GitHub
  home.file.".config/ibus/rime" = {
    source = pkgs.fetchFromGitHub {
      owner = "iDvel";
      repo = "rime-ice";
      rev = "main";
      sha256 = "sha256-nPkwX5ruvCkQZ4yKNVuh72RmbLiEs/lkjb7mPQURVWw=";
    };
    recursive = true;
  };

  # Create rime_ice.custom.yaml to patch editor.bindings
  home.file.".config/ibus/rime/rime_ice.custom.yaml".text = ''
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

  # Configure default.custom.yaml, removing schema_list
  home.file.".config/ibus/rime/default.custom.yaml".text = ''
    patch:
      ascii_composer/good_old_caps_lock: true
      ascii_composer/switch_key:
        Caps_Lock: clear
        Shift_L: noop
        Shift_R: noop
        Control_L: inline
        Control_R: noop
      key_binder/bindings:
        # Emacs style key bindings
        - { when: composing, accept: Control+p, send: Up }          # Previous candidate
        - { when: composing, accept: Control+n, send: Down }        # Next candidate
        - { when: composing, accept: Control+f, send: Right }       # Move cursor forward
        - { when: composing, accept: Control+b, send: Left }        # Move cursor backward
        - { when: composing, accept: Control+a, send: Home }        # Move to start of input
        - { when: composing, accept: Control+e, send: End }         # Move to end of input
        - { when: composing, accept: Control+d, send: Delete }      # Delete character after cursor
        - { when: composing, accept: BackSpace, send: BackSpace }   # Delete character before cursor
        - { when: composing, accept: Control+k, send: delete_to_end }  # Delete to end
  '';

  # Override ibus_rime.yaml
  home.file.".config/ibus/rime/ibus_rime.yaml" = {
    text = ''
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
  };

  # Add activation script to clean .config/ibus/rime except rime_ice.userdb and ensure only one ibus_rime.yaml
  home.activation.cleanRimeConfig = config.lib.dag.entryBefore [ "writeBoundary" ] ''
    if [ -d "$HOME/.config/ibus/rime" ]; then
      # Remove all ibus_rime.yaml files first
      ${pkgs.findutils}/bin/find "$HOME/.config/ibus/rime" -maxdepth 1 -name 'ibus_rime.yaml' -exec rm -f {} +
      # Remove other files except rime_ice.userdb
      ${pkgs.findutils}/bin/find "$HOME/.config/ibus/rime" -maxdepth 1 -not -name 'rime_ice.userdb' -not -name 'ibus_rime.yaml' -exec rm -rf {} +
    fi
  '';
}
