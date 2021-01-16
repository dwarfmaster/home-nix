{ pkgs, ... }:

# Doom needs to be installed manually

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  
  home.packages = with pkgs; [
    gvfs # Necessary for TRAMP support for webdav
    haskellPackages.structured-haskell-mode
    ccls
    texlab
    ripgrep
    sqlite
    wordnet
    fd
  ];


  xdg.configFile."doom/init.el".source = ./init.el;
  xdg.configFile."doom/config.el".source = ./config.el;
  xdg.configFile."doom/packages.el".source = ./packages.el;

  # Set emacsclient as the org-protocol:// url handler
  # TODO use finalPackage
  xdg.dataFile."applications/org-protocol.desktop".text = ''
    [Desktop Entry]
    Name=org-protocol
    Exec=emacsclient %u
    Type=Application
    Terminal=false
    Categories=System;
    MimeType=x-scheme-handler/org-protocol;
  '';

  home.sessionVariables = {
    GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";
  };
}

