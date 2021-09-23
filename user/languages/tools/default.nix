{ config, ... }:

let
  inherit (config.pkgsets) pkgs;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      pijul    # VCS based on sound theory
      gnumake  # Simple generic purpose build system
      ninja    # Simple generic purpose build system
      cmake    # Generic purpose build system
      cloc     # Count code lines
      valgrind # Generic purpose debugger
      gnum4    # Macro preprocessor
      ctags    # Objects indexer for many languages
      autoconf # Makefile generator
      automake # Same
      ;
    perf = pkgs.linuxPackages.perf; # Profile programs
  };
}
