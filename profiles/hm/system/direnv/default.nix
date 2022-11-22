{pkgs, ...}: {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.doom-emacs.config = {
    initModules = {
      tools = ["direnv"];
    };
  };
}
