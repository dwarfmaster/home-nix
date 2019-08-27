let
  # (a -> a) -> a
  fix = f: let x = f x; in x;

  # (a -> b -> b) -> [a] -> b -> b
  fold = f: lst: acc: if builtins.length lst == 0
                        then acc
                        else fold f (builtins.tail lst)
                                  (f builtins.head lst acc);

  # {[...]} -> string -> [...] -> {[...]}
  updateElem = set: attr: val:
    if builtins.hasAttr attr set
      then set // { ${attr} = set.${attr} ++ val; }
      else set // { ${attr} = val; };

  # {[...]} -> {[...]} -> {[...]}
  mergeConcat = set1: set2:
    let attrs = builtins.attrNames set2; in
    let f = attr: set: updateElem set attr (set2.${attr}); in
    fold f attrs set1;

  # {{[...]}} -> {[...]}
  mergeMod = set:
    let lst = builtins.attrValues set; in
    fold mergeConcat lst { };

in {
  inherit fix fold mergeMod;
}

