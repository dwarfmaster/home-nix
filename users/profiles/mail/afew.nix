{ config, lib, pkgs, ... }:

let

  dirs = import ./dirs.nix;

  afewcfg = [
    { filter = "SpamFilter"; spam_tags = "spam"; }
    "KillThreadsFilter"
    { filter = "UnfoldDirectories"; inherit dirs; }
    { filter = "ArchiveSentMailsFilter"; sent_tag = "sent"; }
    "InboxFilter"
  ];

  canonizeCfgItem = item: if builtins.isString item then { filter = item; } else item;

  flattenQuery = query: "(" + builtins.concatStringsSep " or " (map (s: "(${s})") query) + ")";

  flattenDir = attr: dir:
    if builtins.isList dir
    then (if dir == [ ]
          then [ ]
          else [ { tags = [ attr ]; additional = [ ]; query = flattenQuery dir; } ])
    else
      let
        subs = flattenDirs (builtins.removeAttrs dir [ "query" "tags" ]);
        rebased = map (sub: sub // { tags = [ attr ] ++ sub.tags; }) subs;
        query = if dir ? query then flattenQuery dir.query else "";
      in (if query == "" then rebased
          else [ {tags = [ attr ]; additional = dir.tags or [ ]; query = query; } ]
               ++ map (sub: sub // { query = "${query} and ${sub.query}"; }) rebased);

  makeFilter = id: dir: ''
[Filter.${toString id}]
query = '${dir.query}'
tags = +${builtins.concatStringsSep "::" dir.tags} ${builtins.concatStringsSep " " (map (s: "+${s}") dir.additional)}
'';

  flattenDirs = dirs:
    builtins.concatLists (lib.mapAttrsToList
      (attr: value: flattenDir attr value)
      dirs);

  unfoldDirs = dirs:
    (builtins.foldl'
      (acc: d: { id = acc.id + 1; result = "${acc.result}\n${makeFilter acc.id d}"; })
      { id = 0; result = ""; }
      (flattenDirs dirs)).result;

  processItem = item:
    if item.filter == "UnfoldDirectories"
    then unfoldDirs item.dirs
    else "[${item.filter}]\n"
         + builtins.concatStringsSep ""
            (lib.mapAttrsToList
              (name: value: if name != "filter" then "${name} = ${value}\n" else "")
              item);

    finalConfig = builtins.concatStringsSep "\n" (map (i: processItem (canonizeCfgItem i)) afewcfg);

in {
  programs.afew = {
    enable = true;
    extraConfig = finalConfig;
  };
}
