{pkgs, ...}: {
  programs.nushell.enable = true;

  # Integrations
  programs.direnv.enableNushellIntegration = true;

  # Neovim support
  programs.nixvim = {
    autoCmd = [
      {
        event = ["BufNewFile" "BufRead"];
        pattern = ["*.nu"];
        command = "set ft=nu";
      }
    ];
  };
}
