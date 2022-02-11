{ config, pkgs, ... }:

{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      gnuplot     # Plotting programming language
      paraview    # An interface to vtk
      asymptote   # 2D and 3D mathematical drawing
      graphviz    # Graph drawing
      ;
  };
}
