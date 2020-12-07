args:

let

  overlays = import ./overlays args;

in {
  nixos-19-03 = import ./nixos-19.03 (args // { inherit overlays; });
  nixos-19-09 = import ./nixos-19.09 (args // { inherit overlays; });
  nixos-20-03 = import ./nixos-20.03 (args // { inherit overlays; });
  nixos-20-09 = import ./nixos-20.09 (args // { inherit overlays; });
  nixos-unstable = import ./nixos-unstable (args // { inherit overlays; });
}

