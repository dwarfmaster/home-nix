{
  stdenv,
  fetchFromGitHub,
  cmake,
  cln,
  gmp,
  git,
  swig,
  pkg-config,
  readline,
  libantlr3c,
  boost,
  jdk,
  python3,
  antlr3_4,
}:
stdenv.mkDerivation rec {
  pname = "cvc4";
  version = "1.7";

  src = fetchFromGitHub {
    owner = "cvc4";
    repo = "cvc4";
    rev = version;
    sha256 = "0mi3ym9j3y00h66115q3jsj7a1wcxjc94fcsw2lxq899mviywk2z";
  };

  nativeBuildInputs = [pkg-config cmake];
  buildInputs = [gmp git python3.pkgs.toml cln readline swig libantlr3c antlr3_4 boost jdk python3];
  configureFlags = [
    "--enable-language-bindings=c,c++,java"
    "--enable-gpl"
    "--with-cln"
    "--with-readline"
    "--with-boost=${boost.dev}"
  ];

  prePatch = ''
    patch -p1 -i ${./minisat-fenv.patch} -d src/prop/minisat
    patch -p1 -i ${./minisat-fenv.patch} -d src/prop/bvminisat
  '';

  preConfigure = ''
    patchShebangs ./src/
  '';
  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=Production"
  ];

  enableParallelBuilding = true;

  meta = with lib; {
    description = "A high-performance theorem prover and SMT solver";
    homepage = "http://cvc4.cs.stanford.edu/web/";
    license = licenses.gpl3;
    platforms = platforms.unix;
    maintainers = with maintainers; [vbgl thoughtpolice gebner];
  };
}
