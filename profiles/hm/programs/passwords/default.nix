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
  programs.firefox.extensions = [
    pkgs.nur.repos.rycee.firefox-addons.keepassxc-browser
  ];
}
