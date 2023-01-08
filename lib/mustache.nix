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

  renderDir = name: templateDir: vars: let
    json = pkgs.writeText "${name}.json" (builtins.toJSON vars);
  in
    pkgs.runCommandLocal name {} ''
      mkdir -p $out
      dir=$(realpath ${templateDir})
      find "$dir" -type f -print0 | while IFS= read -r -d $'\0' file; do
        rel=$(realpath --relative-to="$dir" "$file")
        reldir=$(dirname "$rel")
        mkdir -p "$out/$reldir"
        ${pkgs.mustache-go}/bin/mustache ${json} $file > "$out/$rel"
      done
    '';
}
