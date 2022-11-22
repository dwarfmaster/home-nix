{lib, ...}: let
  inherit (lib) types mkOption;

  string = types.str;
  number = types.float;
  boolean = types.bool;
  table = types.either (types.attrsOf any) (types.listOf any);

  mkCode = code: {__raw = code;};
  code = let
    sub = types.submodule ({...}: {
      options = {
        __raw = mkOption {
          type = types.lines;
          description = "Lua code";
        };
      };
    });
  in
    types.coercedTo types.lines mkCode sub;

  mkFunction = args: body: let
    bd =
      if body ? __raw
      then body.__raw
      else body;
  in
    mkCode ''
      function(${lib.concatStringsSep ", " args})
        ${lib.replaceStrings ["\n"] ["\n  "] (lib.removeSuffix "\n" body)}
      end
    '';

  any = types.nullOr (types.oneOf [string number boolean table]);

  setupWith = opts:
    types.submodule {
      freeformType = any;
      options = opts;
    };

  setup = setupWith {};

  # Taken from nixvim source and adapted
  toLuaObject = args: let
    inherit
      (lib)
      concatStringsSep
      mapAttrsToList
      stringToCharacters
      filterAttrs
      boolToString
      concatMapStringsSep
      head
      escapeShellArg
      ;
  in
    if builtins.isAttrs args
    then
      if args ? __raw
      then args.__raw
      else
        "{"
        + (concatStringsSep ","
          (mapAttrsToList
            (n: v:
              if head (stringToCharacters n) == "@"
              then toLuaObject v
              else "[${toLuaObject n}] = " + (toLuaObject v))
            (filterAttrs (n: v: !isNull v) args)))
        + "}"
    else if builtins.isList args
    then "{" + concatMapStringsSep "," toLuaObject args + "}"
    else if builtins.isString args
    then
      # This should be enough!
      escapeShellArg args
    else if builtins.isBool args
    then "${boolToString args}"
    else if builtins.isFloat args
    then "${toString args}"
    else if builtins.isInt args
    then "${toString args}"
    else if isNull args
    then "nil"
    else "";
in {
  types = {
    inherit string number boolean table code any;
    inherit setupWith setup;
  };

  nixvim = {
    inherit mkCode mkFunction toLuaObject;
  };
}
