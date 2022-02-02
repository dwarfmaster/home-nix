{ system, tungdil, vraccas, ... }:

{
  name = "trivial";

  nodes = {
    tungdil = { ... }: { imports = tungdil; };
  };

  skipLint = true;
  testScript = ''
    tungdil.start()
    tungdil.wait_for_unit("default.target")
    print("Tungdil succesfully started")
  '';
}
