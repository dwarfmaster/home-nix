{ stdenv, lib, rustPlatform, fetchFromGitHub, ... }:

let

  version = "v0.0.14";

  src = fetchFromGitHub {
    owner  = "theiceshelf";
    repo   = "firn";
    rev    = "${version}";
    sha256 = "1slwcm59znb4w2glwj78904axssg4rh4ih4iiaf1g25y3882cjd2";
  };

  rust-lib = rustPlatform.buildRustPackage {
    pname = "firn-rust-lib";
    inherit version;

    inherit src;
    sourceRoot = "source/rust";
    cargoSha256 = "r2NXShdCfolN32hwN/0GqwEuFg4sdzY5/FhNaT6Sz7A=";

    meta = {
      description = "Org parsing library for firn";
      homepage    = "https://github.com/theiceshelf/firn";
      license     = lib.licenses.epl10;
      maintainers = [ lib.maintainers.dwarfmaster ];
    };
  };

in

stdenv.mkDerivation {
  pname = "firn";
  inherit version;

  inherit src;
  sourceRoot = "source/clojure";

  buildInputs = [ rust-lib ];

  # TODO

  meta = {
    description = "Static Site Generator for Org Mode";
    homepage    = "https://firn.theiceshelf.com/";
    license     = lib.licenses.epl10;
    maintainers = [ lib.maintainers.dwarfmaster ];
  };
}
