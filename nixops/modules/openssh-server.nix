# Configuration of a simple openssh server

{ config, pkgs, ... }:

{
  services.openssh = {
    enable = true;
    forwardX11 = true;
    openFirewall = true;
    allowSFTP = false; # Disable sftp and sshfs
    gatewayPorts = "no"; # Forbid remote hosts to connect
                         # to ports forwarded by the client
    logLevel = "VERBOSE";
    passwordAuthentication = false;
    permitRootLogin = "prohibit-password";
  };
}

