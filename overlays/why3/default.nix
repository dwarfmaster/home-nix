final: prev: {
  why3 = prev.callPackage ./why3 {};
  why3-cvc4 = prev.callPackage ./cvc4 {};
  why3-z3 = prev.callPackage ./z3 {};
  why3-alt-ergo = prev.callPackage ./alt-ergo {};
}
