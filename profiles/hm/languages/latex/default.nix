{
  config,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.python311Packages.dot2tex # Convert graphviz graphs to LaTeX
    pkgs.texlive.combined.scheme-full # All of texlive (including LaTeX and ConTEXt)
    pkgs.texlab # LSP for LaTeX
  ];

  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.vimtex];
    globals = {
      vimtex_view_method = "zathura";
      vimtex_compiler_method = "latexmk";
      vimtex_compiler_silet = true;
    };
  };
}
