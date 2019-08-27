let
  # (a -> a) -> a
  fix = f: let x = f x; in x;

  # (a -> b -> b) -> [a] -> b -> b
  fold = f: lst: acc: if builtins.length lst == 0
                        then acc
                        else fold f (builtins.tail lst)
                                  (f (builtins.head lst) acc);

  # {[...]} -> string -> [...] -> {[...]}
  updateElem = set: attr: val:
    if builtins.hasAttr attr set
      then      if builtins.isList  val then set // { ${attr} = set.${attr} ++ val; }
           else if builtins.isAttrs val then set // { ${attr} = mergeConcat set.${attr} val; }
           else                              set // { ${attr} = val; }
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

  # int -> (a -> a) -> a -> a
  iterate = n: f: x:
    if n == 0 then x
              else iterate (n - 1) f (f x);

  # [string] -> {...} -> ...
  mayAccess = attrs: set:
    if builtins.length attrs == 0
      then set
      else let attr = builtins.head attrs; in
           if builtins.hasAttr attr set
             then mayAccess (builtins.tail attrs) set.${builtins.head attrs}
             else { };

  # (a -> b -> c) -> b -> a -> c
  flip = f: x: y: f y x;

  # [string] -> {...} -> {...}
  removeAttrs = flip builtins.removeAttrs;

in {
  inherit fix fold updateElem mergeConcat mergeMod iterate mayAccess removeAttrs;
}

