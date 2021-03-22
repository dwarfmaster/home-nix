{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    gnupg1           # Old GNU pgp implementation (TODO why is it necessary ?)
    cryptsetup       # Setup DM encrypted disks
  ];

  home.file.".gnupg/gpg-agent.conf".text = ''
# Global parameters
pinentry-program ${pkgs.pinentry-gtk2}/bin/pinentry
write-env-file /tmp/gpg-agent-info-luc

# Cache settings : 30 minutes
default-cache-ttl 600
max-cache-ttl 7200

# Security
enforce-passphrase-constraints
'';

  programs.gpg = {
    enable = true;
    settings = {
      use-agent = true;
    };
  };
}
