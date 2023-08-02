{
  config,
  pkgs,
  ...
}: {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      pass # Unix password manager
      keepassxc # An advanced passphrase manager
      ;
  };

  # Firefox setup
  programs.firefox.profiles = config.lib.firefox.all-profiles {
    extensions = [
      pkgs.nur.repos.rycee.firefox-addons.keepassxc-browser
    ];
  };
}
