{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs) unfree;
  inherit (lib) mapAttrs' nameValuePair concatMapStrings;
  inherit (config.lib.utils) foldOverAttrs attrNameValuePairs;

  colors = config.lib.stylix.colors;
  icons = "${pkgs.numix-icon-theme}/share/icons/Numix/scalable";
  arkenfox = import ./arkenfox.nix {inherit lib;};

  profiles = {
    "Personal" = {
      homepage = "about:blank";
      default = true;
      color = colors.base0F;
      icon = "${icons}/categories/applications-games-symbolic.svg";
      arkenfox = [arkenfox.main arkenfox.safe];
    };
    "Thesis" = {
      homepage = "about:blank";
      color = colors.base0D;
      icon = "${icons}/categories/applications-education-symbolic.svg";
      arkenfox = [arkenfox.main arkenfox.safe];
    };
    "Media" = {
      homepage = "about:blank";
      color = colors.base0E;
      icon = "${icons}/categories/applications-multimedia-symbolic.svg";
      arkenfox = [arkenfox.main];
    };
    "Private" = {
      homepage = "about:blank";
      color = colors.base07;
      icon = "${icons}/emotes/emote-love-symbolic.svg";
      arkenfox = [
        arkenfox.main
        {"1200"."1201"."security.ssl.require_safe_negotiation".value = false;}
      ];
    };
    "Config" = {
      homepage = "about:blank";
      color = colors.base0C;
      icon = "${icons}/categories/applications-system-symbolic.svg";
      arkenfox = [arkenfox.main arkenfox.safe];
    };
    "Shopping" = {
      homepage = "about:blank";
      color = colors.base0A;
      icon = "${icons}/emblems/emblem-system-symbolic.svg";
      arkenfox = [arkenfox.main];
    };
    "Secure" = {
      homepage = "about:blank";
      color = colors.base08;
      icon = "${icons}/status/security-high-symbolic.svg";
      arkenfox = [arkenfox.main arkenfox.hardened];
    };
  };

  # Need to be manually enabled when first launching a new profile
  # To do so, go to about:addons
  extensions = builtins.attrValues {
    # Security
    inherit
      (pkgs.nur.repos.rycee.firefox-addons)
      ublock-origin # Efficient light-wieght ad-blocking
      decentraleyes # Protects against tracking by CDN
      privacy-badger # Auto-learn to block third party trackers and ads
      temporary-containers # Allow opening webpages in specific, temporary containers
      ;

    # Interface
    inherit
      (pkgs.nur.repos.rycee.firefox-addons)
      darkreader
      i-dont-care-about-cookies # Prevent most cookies popups
      don-t-fuck-with-paste
      ;
    inherit
      (unfree.nur.repos.rycee.firefox-addons)
      languagetool
      ;

    # Interaction with the system
    inherit
      (pkgs.nur.repos.rycee.firefox-addons)
      keepassxc-browser
      ;
  };

  buildProfile = id: name: profile: {
    acc = id + 1;
    value = let
      color =
        if profile ? color
        then profile.color
        else colors.base07;
    in {
      inherit name id;
      extensions.packages = extensions;
      settings =
        {
          "browser.startup.homepage" = profile.homepage;
          "browser.uidensity" = "compact";
          "browser.rememberSignons" = false; # Disable password manager
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        }
        // (
          if profile ? settings
          then profile.settings
          else {}
        );
      isDefault =
        if profile ? default
        then profile.default
        else false;
      userChrome = ''
        .tab-background:is([selected], [multiselected]) {
          background-color: #${color} !important;
        }
        .tabbrowser-tab[selected="true"] {
          color: #${colors.base00} !important;
        }
      '';
      arkenfox = lib.mkMerge ([
          {enable = true;}
        ]
        ++ (
          if profile ? arkenfox
          then profile.arkenfox
          else []
        ));
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
    ''
    + concatMapStrings (profile: "echo \"${profile.name}\"\n") (attrNameValuePairs profiles));

  launcher = pkgs.writeShellScriptBin "firefox-launcher" ''
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
    arkenfox = {
      # TODO update
      enable = true;
      version = "107.0";
    };
    profiles = foldOverAttrs 0 buildProfile profiles;
  };
  xdg.dataFile =
    mapAttrs'
    (name: profile: nameValuePair "applications/firefox-${name}.desktop" {text = buildDesktop name profile;})
    profiles
    // {
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
  ];
  applications.browser = "${launcher}/bin/firefox-launcher";

  lib.firefox.all-profiles = attr:
    builtins.listToAttrs
    (builtins.map
      (name: {
        inherit name;
        value = attr;
      })
      (builtins.attrNames profiles));

  # Save profiles
}
