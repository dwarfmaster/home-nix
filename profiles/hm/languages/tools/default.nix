{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      pijul # VCS based on sound theory
      gnumake # Simple generic purpose build system
      ninja # Simple generic purpose build system
      cmake # Generic purpose build system
      cloc # Count code lines
      tokei # Better cloc
      # tokei-pie # Pie representation of tokei TODO
      
      valgrind # Generic purpose debugger
      gnum4 # Macro preprocessor
      ctags # Objects indexer for many languages
      autoconf # Makefile generator
      automake # Same
      ;
    perf = pkgs.linuxPackages.perf; # Profile programs
  };

  programs.doom-emacs.config = {
    initModules = {
      tools = [
        {
          mod = "debugger";
          args = ["lsp"];
        }
        {
          mod = "eval";
          args = ["overlay"];
        }
        {
          mod = "lookup";
          args = ["dictionary" "docsets"];
        }
        {
          mod = "lsp";
          args = ["peek"];
        }
        "make"
      ];
    };
  };
}
