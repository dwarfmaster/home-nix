{
  stdenv,
  fetchFromGitHub,
  python,
  fixDarwinDylibNames,
  javaBindings ? false,
  pythonBindings ? true,
  jdk ? null,
}:
assert javaBindings -> jdk != null;
with lib;
  stdenv.mkDerivation rec {
    pname = "z3";
    version = "4.8.6";

    src = fetchFromGitHub {
      owner = "Z3Prover";
      repo = pname;
      rev = "z3-${version}";
      sha256 = "1sywcqj5y8yp28m4cdvzsgw74kd6zr1s3y1x17ky8pr9prvpvl6x";
    };

    buildInputs = [python fixDarwinDylibNames] ++ optional javaBindings jdk;
    propagatedBuildInputs = [python.pkgs.setuptools];
    enableParallelBuilding = true;

    configurePhase =
      concatStringsSep " " (
        ["${python.interpreter} scripts/mk_make.py --prefix=$out"]
        ++ optional javaBindings "--java"
        ++ optional pythonBindings "--python --pypkgdir=$out/${python.sitePackages}"
      )
      + "\n"
      + "cd build";

    postInstall =
      ''
        mkdir -p $dev $lib
        mv $out/lib     $lib/lib
        mv $out/include $dev/include
      ''
      + optionalString pythonBindings ''
        mkdir -p $python/lib
        mv $lib/lib/python* $python/lib/
        ln -sf $lib/lib/libz3${stdenv.hostPlatform.extensions.sharedLibrary} $python/${python.sitePackages}/z3/lib/libz3${stdenv.hostPlatform.extensions.sharedLibrary}
      '';

    outputs = ["out" "lib" "dev" "python"];

    meta = {
      description = "A high-performance theorem prover and SMT solver";
      homepage = "https://github.com/Z3Prover/z3";
      license = lib.licenses.mit;
      platforms = lib.platforms.x86_64;
      maintainers = with lib.maintainers; [thoughtpolice ttuegel];
    };
  }
