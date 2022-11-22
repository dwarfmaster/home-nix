{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    python = pkgs.python3.withPackages (ppkgs: []);
  };
}
