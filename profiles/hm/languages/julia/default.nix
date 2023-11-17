{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.julia-bin];
  # Julia looks for startup.jl in its DEPOT_PATH
  xdg.dataFile."julia/config/startup.jl".source = ./startup.jl;

  home.sessionVariables = {
    JULIA_DEPOT_PATH = "${config.xdg.dataHome}/julia";
  };
}
