{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
  background-picture = pkgs.copyPathToStore ./bg.png;
in {
  # Woodland theme, from https://github.com/jcornwall/base16-woodland-scheme
  theme.base16 = {
    name = "woodland";
    kind = "dark";
    colors = {
      base00.hex = { r = "23"; g = "1e"; b = "18"; };
      base01.hex = { r = "30"; g = "2b"; b = "25"; };
      base02.hex = { r = "48"; g = "41"; b = "3a"; };
      base03.hex = { r = "9d"; g = "8b"; b = "70"; };
      base04.hex = { r = "b4"; g = "a4"; b = "90"; };
      base05.hex = { r = "ca"; g = "bc"; b = "b1"; };
      base06.hex = { r = "d7"; g = "c8"; b = "bc"; };
      base07.hex = { r = "e4"; g = "d4"; b = "c8"; };
      base08.hex = { r = "d3"; g = "5c"; b = "5c"; };
      base09.hex = { r = "ca"; g = "7f"; b = "32"; };
      base0A.hex = { r = "e0"; g = "ac"; b = "16"; };
      base0B.hex = { r = "b7"; g = "ba"; b = "53"; };
      base0C.hex = { r = "6e"; g = "b9"; b = "58"; };
      base0D.hex = { r = "88"; g = "a4"; b = "d3"; };
      base0E.hex = { r = "bb"; g = "90"; b = "e2"; };
      base0F.hex = { r = "b4"; g = "93"; b = "68"; };
    };
  };

  # Setup doom-emacs theme
  programs.doom = {
    initModules = {
      ui = [
        { mod = "ligatures"; args = [ "extra" "fira" ]; }
        "nav-flash"
        "neotree"
        "ophints"
        { mod = "popup"; args = [ "all" "defaults" ]; }
        "window-select"
        "workspace"
      ];
      tools = [ "rgb" ];
    };
    modules.ui.dwarfmaster-theme = {
      config.source = ./config.el;
    };
  };

  # Set the background when running X
  systemd.user.services = {
    xbackground = {
      Unit = {
        Description = "Set the background image";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.imlibsetroot}/bin/imlibsetroot ${background-picture}";
      };
    };
  };
}
