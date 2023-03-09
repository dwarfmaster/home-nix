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
    # TODO should be in config-hardware, along with the battery and the wifi
    backlight_device = "/sys/class/backlight/intel_backlight";
    wifi_device = "wlp0s20f3";
    inotifywait = "${pkgs.inotify-tools}/bin/inotifywait";
    acpi = "${pkgs.acpi}/bin/acpi";
    bctl = "${pkgs.brightnessctl}/bin/brightnessctl";
    jq = "${pkgs.jq}/bin/jq";
    nmcli = "${pkgs.networkmanager}/bin/nmcli";
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
    cpus = map (cpu: { inherit cpu; })
               (lib.range 0 (osConfig.hardware.specs.threads - 1));
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
  };
  home.packages = [eww-builder];
  xsession.windowManager.bspwm.extraConfig = ''
    ${config.programs.eww.package}/bin/eww open-many \
        statusbar-left \
        statusbar-center \
        statusbar-right
  '';

  programs.nixvim = {
    extraPlugins = [yuck-vim];
  };
}
