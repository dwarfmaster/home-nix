{
  lib,
  pkgs,
  ...
}:
# Inspired by:
# https://pablo.tools/blog/computers/nix-mustache-templates/
{
  render = name: template: vars: let
    json = pkgs.writeText "${name}.json" (builtins.toJSON vars);
  in
    pkgs.runCommandLocal name {} ''
      ${pkgs.mustache-go}/bin/mustache ${json} ${template} > $out
    '';
}
