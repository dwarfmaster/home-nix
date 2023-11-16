{pkgs, ...}: {
  home.packages = [pkgs.postgresql];
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.pgsql-vim];
  };
}
