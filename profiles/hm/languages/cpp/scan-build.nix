{
  buildPythonPackage,
  fetchPypi,
  lib,
}: let
  pname = "scan-build";
  version = "2.0.20";
in
  buildPythonPackage {
    inherit pname version;

    src = fetchPypi {
      inherit pname version;
      sha256 = "f1f9f1dc3daf906ef106077dbac4d872f5740843173dc74070ef3b39da3d0f07";
    };

    meta = with lib; {
      description = "Creates a compilation database by intercepting call to compiler";
      longDescription = ''
        A package designed to wrap a build so that all calls to gcc/clang are
        intercepted and logged into a compilation database and/or piped to the clang
        static analyzer. Includes intercept-build tool, which logs the build, as well
        as scan-build tool, which logs the build and runs the clang static analyzer on
        it.
      '';
      homepage = "https://github.com/rizsotto/scan-build";
      license = [licenses.ncsa];
      maintainers = [maintainers.dwarfmaster];
    };
  }
