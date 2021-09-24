{ config, lib, pkgs, ... }:

let
  inherit (config.pkgsets) pkgs unfree;
in {
  # TODO tweak appearance
  # Consider tabliss

  programs.firefox = {
    enable = true;
    #package = pkgs.firefox;
    enableGnomeExtensions = false;
    extensions = builtins.attrValues {
      # Missing : dont-fuck-with-paste, zotero and vivaldi fox

      # Security
      inherit (pkgs.nur.repos.rycee.firefox-addons)
        ublock-origin        # Efficient light-wieght ad-blocking
        https-everywhere     # Force HTTPS for all connections that supports it
        decentraleyes        # Protects against tracking by CDN
        privacy-badger       # Auto-learn to block third party trackers and ads
        temporary-containers # Allow opening webpages in specific, temporary containers
      ;

      # Interface
      inherit (pkgs.nur.repos.rycee.firefox-addons)
        darkreader
        vim-vixen
        i-dont-care-about-cookies # Prevent most cookies popups
      ;
      inherit (unfree.nur.repos.rycee.firefox-addons)
        languagetool
      ;

      # Interaction with the system
      inherit (pkgs.nur.repos.rycee.firefox-addons)
        org-capture
      ;
    };

    profiles.dwarfmaster = {
      name = "DwarfMaster";
      isDefault = true;
      settings = {
        # TODO create a local page
        "browser.startup.homepage" = "about:blank";
      };
      extraConfig = builtins.readFile ./user.js;
    };
  };
}
