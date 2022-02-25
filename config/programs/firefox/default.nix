{ config, lib, pkgs, ... }:

let
  inherit (pkgs) unfree;
  inherit (lib) mapAttrs' nameValuePair concatMapStrings;
  inherit (lib.utils) foldOverAttrs attrNameValuePairs;

  colors = config.theme.base16.colors;
  icons = "${pkgs.numix-icon-theme}/share/icons/Numix/scalable";

  profiles = {
    "Personal" = {
      homepage = "about:blank";
      default = true;
      color = colors.base0F.hex.rgb;
      icon = "${icons}/categories/applications-games-symbolic.svg";
    };
    "Thesis" = {
      homepage = "about:blank";
      color = colors.base0D.hex.rgb;
      icon = "${icons}/categories/applications-education-symbolic.svg";
    };
    "Media" = {
      homepage = "about:blank";
      color = colors.base0E.hex.rgb;
      icon = "${icons}/categories/applications-multimedia-symbolic.svg";
    };
    "Private" = {
      homepage = "about:blank";
      color = colors.base07.hex.rgb;
      icon = "${icons}/emotes/emote-love-symbolic.svg";
    };
    "Config" = {
      homepage = "about:blank";
      color = colors.base0C.hex.rgb;
      icon = "${icons}/categories/applications-system-symbolic.svg";
    };
    "Shopping" = {
      homepage = "about:blank";
      color = colors.base0A.hex.rgb;
      icon = "${icons}/emblems/emblem-system-symbolic.svg";
    };
    "Secure" = {
      homepage = "about:blank";
      extraConfig = builtins.readFile ./user.js;
      color = colors.base08.hex.rgb;
      icon = "${icons}/status/security-high-symbolic.svg";
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

  home.packages = [ launcher ];
  applications.browser = "${launcher}/bin/firefox-launcher";
}
