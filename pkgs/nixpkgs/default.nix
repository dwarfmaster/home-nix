args:

let

  overlays = import ./overlays args;

in {
  nixos-19-03 = import ./nixos-19.03 args;
  nixos-19-09 = import ./nixos-19.09 (args // { overlays = overlays.v19-09; });
  nixos-20-03 = import ./nixos-20.03 (args // { overlays = overlays.v20-03; });
  nixos-20-09 = import ./nixos-20.09 (args // { overlays = overlays.v20-09; });
  nixos-unstable = import ./nixos-unstable (args // { overlays = overlays.unstable; });
}

