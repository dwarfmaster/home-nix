self: super: let
  inherit (super) callPackage;
in {
  reupload = callPackage ./reupload {};
}
