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

  scripts = {
    # mute-listener = "${pkgs.writeShellScript "mute" (builtins.readFile ./scripts/mute.sh)}";
    # volume-listener = "${pkgs.writeShellScript "volume" (builtins.readFile ./scripts/volume.sh)}";
    volume-listener = "${writeRakuScript "volume" (builtins.readFile ./scripts/volume.raku)}";
  };

  colors = config.colorScheme.colors;
  variables = colors // (import ./constants.nix) // scripts;
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
