{
  config,
  pkgs,
  ...
}: {
  # Set the background using feh
  stylix.targets.feh.enable = true;

  # Cursor theme
  stylix.cursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Amber";
    size = 24;
  };

  # Setup doom-emacs theme
  programs.doom-emacs.config = {
    initModules = {
      ui = [
        {
          mod = "ligatures";
          args = ["extra"];
        }
        "nav-flash"
        "neotree"
        "ophints"
        {
          mod = "popup";
          args = ["all" "defaults"];
        }
        "window-select"
        "workspace"
      ];
      tools = ["rgb"];
    };
    modules.ui.dwarfmaster-theme = {
      config.source = ./config.el;
    };
  };
}
