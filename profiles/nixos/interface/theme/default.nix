{
  config,
  pkgs,
  ...
}: let
  nerdfonts = pkgs.nerd-fonts.fira-code;
  colors = config.lib.stylix.colors;
  bg = pkgs.runCommand "bg.png" {} ''
    cp ${./bg.svg} bg.svg
    sed -i 's/#202330/#${colors.base00}/' bg.svg
    sed -i 's/#262a3b/#${colors.base01}/' bg.svg
    sed -i 's/#353350/#${colors.base02}/' bg.svg
    sed -i 's/#505674/#${colors.base03}/' bg.svg
    sed -i 's/#697589/#${colors.base04}/' bg.svg
    sed -i 's/#747a9e/#${colors.base05}/' bg.svg
    sed -i 's/#a9adc3/#${colors.base06}/' bg.svg
    sed -i 's/#c6c6c6/#${colors.base07}/' bg.svg
    sed -i 's/#bb585d/#${colors.base08}/' bg.svg
    sed -i 's/#c06d54/#${colors.base09}/' bg.svg
    sed -i 's/#c69e53/#${colors.base0A}/' bg.svg
    sed -i 's/#97b36d/#${colors.base0B}/' bg.svg
    sed -i 's/#6aacc6/#${colors.base0C}/' bg.svg
    sed -i 's/#6584c6/#${colors.base0D}/' bg.svg
    sed -i 's/#9b71b6/#${colors.base0E}/' bg.svg
    sed -i 's/#c64157/#${colors.base0F}/' bg.svg
    ${pkgs.inkscape}/bin/inkscape -w 800 -h 800 bg.svg -o bg.png
    ${pkgs.imagemagick}/bin/convert bg.png -background '#${colors.base00}' -gravity center -extent 1920x1080 $out
  '';
in {
  stylix = {
    enable = true;
    autoEnable = false;
    homeManagerIntegration = {
      followSystem = true;
      autoImport = true;
    };
    image = "${bg}";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tomorrow-night.yaml";
    polarity = "dark";

    # Fonts
    fonts = {
      # I dont care about serif fonts, I'll reuse the sans-serif one
      serif = {
        name = "Manrope3 Medium";
        package = pkgs.manrope;
      };
      sansSerif = {
        name = "Manrope3 Medium";
        package = pkgs.manrope;
      };
      monospace = {
        name = "FiraCode Nerd Font Mono";
        package = nerdfonts;
      };
      emoji = {
        name = "OpenMoji Color";
        package = pkgs.openmoji-color;
      };
    };

    # Cursor theme
    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Amber";
      size = 24;
    };

    targets.console.enable = true;
  };
}
