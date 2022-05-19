{ config, lib, pkgs, ... }:

let
  inherit (pkgs) unfree;
  inherit (lib) mapAttrs' nameValuePair concatMapStrings;
  inherit (lib.utils) foldOverAttrs attrNameValuePairs;

  colors = config.theme.base16.colors;
  icons = "${pkgs.numix-icon-theme}/share/icons/Numix/scalable";
  arkenfox = import ./arkenfox.nix { inherit lib; };

  profiles = {
    "Personal" = {
      homepage = "about:blank";
      default = true;
      color = colors.base0F.hex.rgb;
      icon = "${icons}/categories/applications-games-symbolic.svg";
      arkenfox = [ arkenfox.main arkenfox.safe ];
    };
    "Thesis" = {
      homepage = "about:blank";
      color = colors.base0D.hex.rgb;
      icon = "${icons}/categories/applications-education-symbolic.svg";
      arkenfox = [ arkenfox.main arkenfox.safe ];
    };
    "Media" = {
      homepage = "about:blank";
      color = colors.base0E.hex.rgb;
      icon = "${icons}/categories/applications-multimedia-symbolic.svg";
      arkenfox = [ arkenfox.main ];
    };
    "Private" = {
      homepage = "about:blank";
      color = colors.base07.hex.rgb;
      icon = "${icons}/emotes/emote-love-symbolic.svg";
      arkenfox = [
        arkenfox.main
        { "1200"."1201"."security.ssl.require_safe_negotiation".value = false; }
      ];
    };
    "Config" = {
      homepage = "about:blank";
      color = colors.base0C.hex.rgb;
      icon = "${icons}/categories/applications-system-symbolic.svg";
      arkenfox = [ arkenfox.main arkenfox.safe ];
    };
    "Shopping" = {
      homepage = "about:blank";
      color = colors.base0A.hex.rgb;
      icon = "${icons}/emblems/emblem-system-symbolic.svg";
      arkenfox = [ arkenfox.main ];
    };
    "Secure" = {
      homepage = "about:blank";
      color = colors.base08.hex.rgb;
      icon = "${icons}/status/security-high-symbolic.svg";
      arkenfox = [ arkenfox.main arkenfox.hardened ];
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
        "browser.rememberSignons" = false; # Disable password manager
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      } // (if profile ? settings then profile.settings else { });
      isDefault = if profile ? default then profile.default else false;
      userChrome = ''
        .tab-background[selected="true"] {
          background: #${color} !important;
        }
        .tabbrowser-tab[selected="true"] {
          color: #${colors.base00.hex.rgb} !important;
        }
      '';
      arkenfox = lib.mkMerge ([
        { enable = true; }
      ] ++ (if profile ? arkenfox then profile.arkenfox else [ ]));
    };
  };

  buildDesktop = name: profile: ''
    [Desktop Entry]
    Name=Firefox - ${name}
    Exec=${config.programs.firefox.package}/bin/firefox -P "${name}"
    Type=Application
    Terminal=False
    Icon=${profile.icon}
  '';

  rofi-script = pkgs.writeScript "rofi-firefox" (''
    if test "$#" -eq 1; then
        coproc (${config.programs.firefox.package}/bin/firefox "$URL_TO_OPEN" -P "$@" > /dev/null 2>&1)
        exit 0
    fi
  '' + concatMapStrings (profile: "echo \"${profile.name}\"\n") (attrNameValuePairs profiles));

  launcher = pkgs.writeScriptBin "firefox-launcher" ''
    if test "$#" -eq 1; then
      url="$@"
    else
      url="about:blank"
    fi
    URL_TO_OPEN="$url" ${pkgs.rofi}/bin/rofi -modi "Firefox Profile:${rofi-script}" -show "Firefox Profile"
  '';

in {
  programs.firefox = {
    enable = true;
    enableGnomeExtensions = false;
    enableArkenfox = true;
    arkenfoxVersion = "100.0";
    # Need to be manually enabled when first launching a new profile
    # To do so, go to about:addons
    extensions = builtins.attrValues {
      # Missing : dont-fuck-with-paste, zotero and wallabagger

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
        # wallabagger
      ;
    };

    profiles = foldOverAttrs 0 buildProfile profiles;
  };
  xdg.dataFile = mapAttrs'
    (name: profile: nameValuePair "applications/firefox-${name}.desktop" { text = buildDesktop name profile; })
    profiles // {
      "applications/firefox-launcher.desktop".text = ''
        [Desktop Entry]
        Name=Web Browser
        Exec=${launcher}/bin/firefox-launcher
        Type=Application
        Terminal=False
        Icon=firefox
      '';
    };

  home.packages = [
    launcher
    pkgs.nyxt # Minimalistic browser (replace firefox ?)
  ];
  applications.browser = "${launcher}/bin/firefox-launcher";
}
