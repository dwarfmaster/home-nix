{ config, lib, pkgs, ... }:

{
  services.redis = {
    enable = true;
    # port = 0;
    # unixSocket = "/run/redis/redis.sock";
    # unixSocketPerm = 770;
    openFirewall = false;
  };
  users.groups.redis.members = [ config.services.nginx.user ];
}