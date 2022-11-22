{
  config,
  pkgs,
  ...
}: let
  implicitcad = pkgs.haskellPackages.implicit;
in {
  home.packages = builtins.attrValues {
    inherit implicitcad; # haskell-based CAD
    inherit
      (pkgs)
      # 3D
      
      solvespace # minimalistic CAD
      freecad # generic CAD
      openscad # text-based CAD
      
      # Elec
      
      kicad # PCB prototyping and design
      ngspice # Electronic circuits simulator
      horizon-eda # PCB prototyping and design
      librepcb # PCB prototyping and design
      
      # GCode
      
      slic3r # GCode generator for 3D printers
      # TODO dxf2gcode  # gcode generator for CNC
      
      # TODO camotics   # GCode viewer/CNC simulator
      
      # Misc
      
      leocad # LEGO models designer
      libxslt # Tools to work with xsl files, used by kicad
      ;
  };
}
