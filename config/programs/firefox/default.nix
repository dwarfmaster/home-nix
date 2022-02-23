{ config, lib, pkgs, ... }:

let
  inherit (pkgs) unfree;
  inherit (lib.utils) foldOverAttrs;

  colors = config.theme.base16.colors;

  profiles = {
    "Personal" = {
      homepage = "about:blank";
      default = true;
      color = colors.base0F.hex.rgb;
    };
    "Thesis" = {
      homepage = "about:blank";
      color = colors.base0D.hex.rgb;
    };
    "Media" = {
      homepage = "about:blank";
      color = colors.base0E.hex.rgb;
    };
    "Private" = {
      homepage = "about:blank";
      color = colors.base07.hex.rgb;
    };
    "Config" = {
      homepage = "about:blank";
      color = colors.base0C.hex.rgb;
    };
    "Shopping" = {
      homepage = "about:blank";
      color = colors.base0A.hex.rgb;
    };
    "Secure" = {
      homepage = "about:blank";
      extraConfig = builtins.readFile ./user.js;
      color = colors.base08.hex.rgb;
    };
  };

  buildProfile = id: name: profile: {
    acc = id + 1;
    value = let
      color = if profile ? color then profile.color else colors.base07.hex.rgb;
    in {
      inherit name id;
      settings = {
        "browser.startup.homepage" = profile.homepage;
        "browser.uidensity" = "compact";
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      } // (if profile ? settings then profile.settings else { });
      extraConfig = if profile ? extraConfig then profile.extraConfig else "";
      isDefault = if profile ? default then profile.default else false;
      userChrome = ''
        .tab-background[selected="true"] {
          background: #${color} !important;
        }
        .tabbrowser-tab[selected="true"] {
          color: #${colors.base00.hex.rgb} !important;
        }
      '';
    };
  };

in {
  programs.firefox = {
    enable = true;
    enableGnomeExtensions = false;
    # Need to be manually enabled when first launching a new profile
    # To do so, go to about:addons
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

    profiles = foldOverAttrs 0 buildProfile profiles;
    # profiles.dwarfmaster = {
    #   name = "DwarfMaster";
    #   isDefault = true;
    #   settings = {
    #     # TODO create a local page
    #     "browser.startup.homepage" = "about:blank";
    #   };
    #   extraConfig = builtins.readFile ./user.js;
    # };
  };

  applications.browser = "${config.programs.firefox.package}/bin/firefox";
}
