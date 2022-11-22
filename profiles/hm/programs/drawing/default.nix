{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      gimp # scalar image editor
      inkscape # vectorial image editor
      gv # PS utilities
      ;
  };
}
