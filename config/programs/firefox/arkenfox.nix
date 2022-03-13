{ lib, ... }:

{
  main = {
    "0000".enable = true;
    "0100" = {
      enable = true;
      # Allow setting homepage
      "0102"."browser.startup.page".value = 1;
    };
    "0200" = {
      enable = true;
      "0210"."intl.accept_languages".value = "en-IE, en";
    };
    "0300".enable = true;
    # We keep safebrowsing
    "0400".enable = false;
    "0600" = {
      enable = true;
      "0610"."browser.send_pings".enable = true;
    };
    "0700" = {
      enable = true;
      # Keep IPv6 enabled
      "0701".enable = false;
    };
    "0800" = {
      enable = true;
      # Keep using url bar as search bar
      "0801"."keyword.enabled".value = true;
      "0810"."browser.formfill.enable".value = true;
    };
    "0900".enable = true;
    "1000" = {
      enable = true;
      # Enable disk cache for performance reasons
      "1001"."browser.cache.disk.enable".enable = true;
      "1001"."browser.cache.disk.enable".value  = true;
    };
    "1200".enable = true;
    # I don't use container tabs
    "1700".enable = false;
    "2600" = {
      enable = true;
      # The recent documents feature is useful
      "2653".enable = false;
    };
    "2700".enable = true;
  };

  safe = {
    "1400".enable = true;
    "1600".enable = true;
    "2400".enable = true;
    "2600" = {
      "2615"."permissions.default.shortcuts".enable = true;
    };
    "2800" = {
      enable = true;
      "2811"."privacy.clearOnShutdown.history".enable = false;
      "2813"."privacy.clearOnShutdown.openWindows".enable = true;
    };
  };

  hardened = {
    "0700"."0701".enable = lib.mkForce true;
    "0800" = {
      "0801"."keyword.enabled".value = lib.mkForce false;
      "0810"."browser.formfill.enable".value = lib.mkForce false;
    };
    # Disable WebRTC
    "2000" = {
      enable = true;
      "2001"."media.peerconnection.enabled".enable = true;
    };
    "2800" = {
      enable = true;
      "2811"."privacy.clearOnShutdown.history".enable = lib.mkForce true;
    };
    # Fingerprinting is overkill for other usages
    "4500".enable = true;
  };
}
