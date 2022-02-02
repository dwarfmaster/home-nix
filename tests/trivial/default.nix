{ system, tungdil, vraccas, ... }:

{
  name = "trivial";
  inherit system;

  nodes = {
    tungdil = { ... }: { };
  };

  skipLint = true;
  testScript = ''
    tungdil.start()
    tungdil.wait_for_unit("default.target")
    print("Tungdil succesfully started")
  '';
}
