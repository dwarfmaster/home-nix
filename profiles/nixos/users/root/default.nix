{
  pkgs,
  config,
  ...
}: let
  passwd = import ./password.nix;
in {
  users.users.root.hashedPassword = passwd;

  assertions = [
    {
      assertion = passwd != "";
      message = "root password must be set !";
    }
  ];
}
