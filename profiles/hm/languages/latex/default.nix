{
  config,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.python27Packages.dot2tex # Convert graphviz graphs to LaTeX
    pkgs.texlive.combined.scheme-full # All of texlive (including LaTeX and ConTEXt)
    pkgs.lua53Packages.digestif # Code analyze for TeX
    pkgs.texlab # LSP for LaTeX
  ];

  programs.doom-emacs.config = {
    initModules = {
      lang = [
        {
          mod = "latex";
          args = ["latexmk" "cdlatex" "lsp"];
        }
      ];
    };
  };

  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.vimtex];
    globals = {
      vimtex_view_method = "zathura";
      vimtex_compiler_method = "latexmk";
      vimtex_compiler_silet = true;
    };
  };
}
