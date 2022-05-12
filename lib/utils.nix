{ lib, ... }:
let
  inherit (builtins)
    attrNames isAttrs readDir listToAttrs attrValues;
  inherit (lib)
    filterAttrs hasSuffix hasPrefix mapAttrs' mapAttrsToList
    nameValuePair removeSuffix;

  # mapFilterAttrs ::
  #   (name -> value -> bool )
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs = sieve: f: attrs: filterAttrs sieve (mapAttrs' f attrs);

  # Generate an attribute set by mapping a function over a list of values.
  genAttrs' = values: f: listToAttrs (map f values);

  importProfiles' = dir: content:
    let
      r =
        mapFilterAttrs
          (_: v: v != null)
          (n: v:
            if v == "directory"
            then nameValuePair n (importProfiles "${dir}/${n}")
            else nameValuePair "" null)
          content;
    in (if r ? "default" then r.default else r);

  importProfiles = dir:
    let content = readDir dir;
    in if content ? "default.nix"
       then import "${dir}"
       else importProfiles' dir content;

in
{
  inherit mapFilterAttrs genAttrs' importProfiles;

  recImport = { dir, _import ? base: import "${dir}/${base}.nix" }:
    mapFilterAttrs
      (_: v: v != null)
      (n: v:
        if n != "default.nix" && hasSuffix ".nix" n && v == "regular"
        then
          let name = removeSuffix ".nix" n; in nameValuePair (name) (_import name)
        else
          nameValuePair ("") (null))
      (readDir dir);

  recImportDir = { dir, _import ? path: import path }:
    mapFilterAttrs
      (_: v: v == "directory")
      (n: v: nameValuePair n (_import "${dir}/${n}"))
      (readDir dir);

  # Convert a list to file paths to attribute set
  # that has the filenames stripped of nix extension as keys
  # and imported content of the file as value.
  pathsToImportedAttrs = paths:
    genAttrs' paths (path: {
      name = removeSuffix ".nix" (baseNameOf path);
      value = import path;
    });

  # List files in directory that are not hidden
  readVisible = dir:
    filterAttrs
      (name: _: !(hasPrefix "." name))
      (readDir dir);

  # x -> (x -> string -> v -> { acc : x; value: v }) -> attrset -> attrset
  foldOverAttrs = init: op: attrs:
    (builtins.foldl'
      (acc: attr: let nattr = op acc.acc attr.name attr.value; in {
                        acc = nattr.acc;
                        value = acc.value // { "${attr.name}" = nattr.value; };
                      })
      { acc = init; value = { }; }
      (mapAttrsToList nameValuePair attrs)).value;

  # Returns a list of name value pairs for an attrset
  attrNameValuePairs = mapAttrsToList nameValuePair;
}
