{ lib, mkCoqDerivation, coq, version ? null }:

mkCoqDerivation {
  pname = "HoTT";
  domain = "github.com";
  owner = "HoTT";
  inherit version;
  defaultVersion = lib.switch coq.coq-version [
    { case = [ "8.10" ];   out = "8.10"; }
    { case = [ "8.11" ];   out = "8.11"; }
    { case = [ "8.12" ];   out = "8.12"; }
    { case = [ "8.13" ];   out = "8.13"; }
    { case = [ "8.13.1" ]; out = "8.13.1"; }
    { case = [ "8.14" ];   out = "8.14"; }
    { case = [ "8.15" ];   out = "8.15"; }
  ] null;
  releaseRev = v: "V${v}";
  release."8.10"  .sha256 = "0ahfgqnf92fxcmrrvcam9z5v3c5sim38h8vh5pyfdnjznqrhjb3d";
  release."8.11"  .sha256 = "0070xjg2ph2hxi16ggvdnhn2b9gd0qbv49fazncz386hxb5v1cav";
  release."8.12"  .sha256 = "034ciyyzfqb8qh2fj9zpg5zg2kbv0f6bx0snc3bmbcb8gi6lgypd";
  release."8.13"  .sha256 = "1vg0czclp8cvcarfahdp5qlym3296ia8p2129vi2pyv6p67z82ch";
  release."8.13.1".sha256 = "1n0n6rqkw5g4dl26s1sd7g5jgq8gnr9n1xs125pzqjx843c0zri1";
  release."8.14"  .sha256 = "1bdy6864lg8wpzc2d67k1dxjr0q66zdl4gkhfil37ccqk7df8igf";
  release."8.15"  .sha256 = "1n4zah2687l80v3sv47b5k08sdglcfkwnfqg97vkkbk7jm2s5xr5";

  meta = {
    description = "A formalisation of Homotopy Type Theory in Coq";
    license = lib.licenses.bsd2;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
