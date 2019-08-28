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

  # [string] -> {...} -> a -> ...
  defAccess = attrs: set: def:
    if builtins.length attrs == 0
      then set
      else let attr = builtins.head attrs; in
           if builtins.hasAttr attr set
             then mayAccess (builtins.tail attrs) set.${builtins.head attrs}
             else def;

  # [string] -> {...} -> ...
  mayAccess = attrs: set: defAccess attrs set { };

  # (a -> b -> c) -> b -> a -> c
  flip = f: x: y: f y x;

  # [string] -> {...} -> {...}
  removeAttrs = flip builtins.removeAttrs;


  # Convert a NIX expression to JSON in a very basic way
  toJSON = set: toJSONid 0 set + "\n\n";

  toJSONid = n: obj: with builtins;
         if isAttrs obj  then attrJSON n obj
    else if isBool obj   then boolJSON n obj
    else if isInt obj    then intJSON  n obj
    else if isList obj   then listJSON n obj
    else if isString obj then strJSON  n obj
    else abort "Tried to convert invalid type to JSON";

  tabs = n: if n == 0 then ""
                      else "  " + tabs (n - 1);

  boolJSON = n: b:
    if b then "true" else "false";

  intJSON = n: i: "\"" + builtins.toString i + "\"";

  strJSON = n: str: "\"" + str + "\"";

  attrJSONhlp = n: set: attrs: with builtins;
    if length attrs == 0
      then ""
      else let hd = head attrs; tl = tail attrs;
           in tabs n + ", \"" + hd + "\" : " + toJSONid (n + 1) set.${hd} + "\n"
                     + attrJSONhlp n set tl;

  attrJSON = n: set: with builtins;
    let attrs = attrNames set; in
    if length attrs == 0
      then "{ }"
      else let hd = head attrs; tl = tail attrs;
           in "{\n" + tabs n + "  \"" + hd + "\" : " + toJSONid (n + 1) set.${hd} + "\n"
            + attrJSONhlp n set tl
            + tabs n + "}";

  listJSONhlp = n: lst: with builtins;
    if length lst == 0
      then ""
      else let hd = head lst; tl = tail lst;
           in tabs n + ", " + toJSONid (n + 1) hd + "\n"
                     + listJSONhlp n tl;

  listJSON = n: lst: with builtins;
    if length lst == 0
      then "[ ]"
      else let hd = head lst; tl = tail lst;
           in "[\n" + tabs n + "  " + toJSONid (n + 1) hd + "\n"
            + listJSONhlp n tl
            + tabs n + "]";

in {
  inherit fix fold updateElem mergeConcat mergeMod iterate defAccess mayAccess removeAttrs toJSON;
}

