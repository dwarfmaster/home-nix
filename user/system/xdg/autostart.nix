{ config, lib, pkgs, ... }:

# Spec: https://specifications.freedesktop.org/autostart-spec/autostart-spec-latest.html
# Won't support:
# - autostart directories (if something must be started automatically at some point, I will set it from nix)
# - autostart when mounting (too much of a security risk, even with prompt)
# TODO
# - autoopen with prompt on a list of whitelisted file types
{

}
