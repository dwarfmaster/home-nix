{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      viking # GPS traces editor
      qgis
      ;
  };

  home.persistence."/persists/luc".directories = [
    ".config/viking"
    ".cache/viking"
  ];
}
