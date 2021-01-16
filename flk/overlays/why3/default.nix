self: super: {
  why3 = super.callPackage ./why3 { };
  why3-cvc4 = super.callPackage ./cvc4 { };
  why3-z3 = super.callPackage ./z3 { };
  why3-alt-ergo = super.callPackage ./alt-ergo { };
}

