{ config, lib, pkgs, ... }:

{
  programs.afew = {
    enable = true;
    extraConfig = ''
[SpamFilter]
[KillThreadsFilter]
[ListMailsFilter]
[ArchiveSentMailsFilter]
[InboxFilter]
'';
  };
}
