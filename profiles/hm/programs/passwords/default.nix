{ config, pkgs, ... }:

{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      pass # Unix password manager
      keepassxc # An advanced passphrase manager
      ;
  };

  # Firefox setup
  programs.firefox.extensions = [
    pkgs.nur.repos.rycee.firefox-addons.keepassxc-browser
  ];
  home.file.".mozilla/native-messaging-hosts/org.keepassxc.keepassxc_browser.json".text = builtins.toJSON {
    allowed_extensions = [
      "keepassxc-browser@keepassxc.org"
    ];
    description = "KeePassXC integration with native messaging support";
    name = "org.keepassxc.keepassxc_browser";
    path = "${pkgs.keepassxc}/bin/keepassxc-proxy";
    type = "stdio";
  };
}
