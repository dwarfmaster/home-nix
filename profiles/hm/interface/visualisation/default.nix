{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      gnuplot # Plotting programming language
      # paraview # An interface to vtk
      
      # TODO asymptote not available on aarch64 because of gsl
      
      graphviz # Graph drawing
      ;
  };
}
