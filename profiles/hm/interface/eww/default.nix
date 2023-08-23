{
  config,
  osConfig,
  pkgs,
  lib,
  ...
}: let
  yuck-vim = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "yuck.vim";
    version = "9b5e0370f70cc30383e1dabd6c215475915fe5c3";
    src = pkgs.fetchFromGitHub {
      owner = "elkowar";
      repo = "yuck.vim";
      rev = "9b5e0370f70cc30383e1dabd6c215475915fe5c3";
      sha256 = "1mkf0vd8vvw1njlczlgai80djw1n1a7dl1k940l089d3vvqr5dhp";
    };
  };

  writeRakuScript = name: text:
    pkgs.writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!${pkgs.rakudo}/bin/raku
        ${text}
      '';
      checkPhase = ''
        ${pkgs.rakudo}/bin/raku -c "$target"
      '';
    };

  context = {
    backlight_device = osConfig.hardware.specs.backlightDevice;
    wifi_device = osConfig.hardware.specs.wifiDevice;
    inotifywait = "${pkgs.inotify-tools}/bin/inotifywait";
    acpi = "${pkgs.acpi}/bin/acpi";
    bctl = "${pkgs.brightnessctl}/bin/brightnessctl";
    jq = "${pkgs.jq}/bin/jq";
    nmcli = "${pkgs.networkmanager}/bin/nmcli";
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
    threads =
      map (thread: {inherit thread;})
      (lib.range 0 (osConfig.hardware.specs.threads - 1));
    cores =
      map (core: {inherit core;})
      (lib.range 0 (osConfig.hardware.specs.cores - 1));
    bat = osConfig.hardware.specs.battery;
    desktops = 
      map (desktop: {inherit desktop;})
      ["l4" "l3" "l2" "l1" "l0" "r0" "r1" "r2" "r3" "r4"];
  };
  makeScript = name: file:
    writeRakuScript name (builtins.readFile (config.lib.mustache.render name file context));

  scripts = {
    volume-listener = "${makeScript "volume" ./scripts/volume.raku}";
    backlight-listener = "${makeScript "backlight" ./scripts/backlight.raku}";
    wifi-listener = "${makeScript "wifi" ./scripts/wifi.raku}";
    locker = "${config.applications.locker}";
  };

  colors = {
    inherit
      (config.lib.stylix.colors)
      base00
      base01
      base02
      base03
      base04
      base05
      base06
      base07
      base08
      base09
      base0A
      base0B
      base0C
      base0D
      base0E
      base0F
      ;
  };
  variables = colors // (import ./constants.nix) // scripts // context;
  json = pkgs.writeText "base16.json" (builtins.toJSON variables);
  eww-builder-script = config.lib.mustache.render "eww-builder" ./eww-builder.sh {
    json = "${json}";
    mustache = "${pkgs.mustache-go}/bin/mustache";
    inkscape = "${pkgs.inkscape}/bin/inkscape";
    jq = "${pkgs.jq}/bin/jq";
  };
  eww-builder = pkgs.writeShellScriptBin "eww-builder" (builtins.readFile "${eww-builder-script}");

  eww-config = pkgs.runCommandLocal "eww" {} ''
    ${eww-builder}/bin/eww-builder ${./status_bar} $out
  '';
in {
  programs.eww = {
    enable = true;
    configDir = eww-config;
    package = pkgs.unstable.eww;
  };
  home.packages = [eww-builder];

  programs.nixvim = {
    extraPlugins = [yuck-vim];
  };
}
