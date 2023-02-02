{ config, pkgs, ... }:

let
  nerdfonts = pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; };
  font = {
    package = nerdfonts;
    name = "FiraCode Nerd Font Mono";
  };
  colors = {
    base00 = "231e18";
    base01 = "302b25";
    base02 = "48413a";
    base03 = "9d8b70";
    base04 = "b4a490";
    base05 = "cabcb1";
    base06 = "d7c8bc";
    base07 = "e4d4c8";
    base08 = "d35c5c";
    base09 = "ca7f32";
    base0A = "e0ac16";
    base0B = "b7ba53";
    base0C = "6eb958";
    base0D = "88a4d3";
    base0E = "bb90e2";
    base0F = "b49368";
  };
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
    autoEnable = false;
    homeManagerIntegration.enable = true;
    homeManagerIntegration.disableImport = true;
    fonts = {
      serif = font;
      sansSerif = font;
      monospace = font;
    };
    image = "${bg}";
  };
}
