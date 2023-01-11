{
  config,
  pkgs,
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
    };

  context = {
    # TODO should be in config-hardware, along with the battery
    backlight_device = "/sys/class/backlight/intel_backlight";
    inotifywait = "${pkgs.inotify-tools}/bin/inotifywait";
    acpi = "${pkgs.acpi}/bin/acpi";
    bctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  };
  makeScript = name: file:
    writeRakuScript name (builtins.readFile (config.lib.mustache.render name file context));

  scripts = {
    volume-listener = "${makeScript "volume" ./scripts/volume.raku}";
    backlight-listener = "${makeScript "backlight" ./scripts/backlight.raku}";
  };

  colors = config.colorScheme.colors;
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
  home.packages = [ eww-builder ];
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
