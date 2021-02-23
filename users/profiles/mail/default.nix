{ config, lib, pkgs, ... }:

let

  realName = "Luc Chabassier";

  maildir = { path = "dwarfmaster"; };

  getmail = {
    enable = true;
    delete = true;
    readAll = true;
    mailboxes = [ "ALL" ];
    destinationCommand = "${pkgs.maildrop}/bin/maildrop";
  };

in {
  imports = [
    ./mua.nix
    ./afew.nix
  ];

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    maildir.synchronizeFlags = true;
    new.tags = [ "new" ];
    search.excludeTags = [ "spam" ];
  };

  accounts.email.maildirBasePath = "/home/luc/mail";
  accounts.email.accounts = {
    dwarfmaster = {
      address = "luc@dwarfmaster.net";
      inherit realName;
      userName = "luc@dwarfmaster.net";
      aliases = [ "root@dwarfmaster.net" ];
      primary = true;

      inherit maildir;
      notmuch.enable = true;
      flavor = "plain";
      passwordCommand = "pass mail/dwarfmaster.net/luc";
      astroid.enable = true;

      # TODO setup gpg

      imap = {
        host = "dwarfmaster.net";
        port = 143;
        tls = {
          enable = true;
          useStartTls = true;
        };
      };

      mbsync = {
        enable = true;
        create = "both";
        expunge = "none";
        remove = "none";
        patterns = [ "*" ];
      };

      msmtp.enable = true;
      smtp = {
        host = "dwarfmaster.net";
        port = 587;
        tls = {
          enable = true;
          useStartTls = true;
        };
      };
    };

    # "ens" = {
    #   address = "luc.chabassier@ens.fr";
    #   aliases = [ "luc.chabassier@ens.psl.eu" "chabassi@clipper.ens.fr" "chabassi@clipper.ens.psl.eu" ];
    #   inherit realName getmail maildir;
    # };

    # "gmail" = {
    #   address = "luc.chabassier@gmail.com";
    #   inherit realName getmail maildir;
    #   flavor = "gmail.com";
    # };

    # "mailoo" = {
    #   address = "luc.linux@mailoo.org";
    #   inherit realName getmail maildir;
    # };

    # "hotmail" = {
    #   address = "luc.toulouse@hotmail.fr";
    #   aliases = [ "luc.toulouse@outlook.com" ];
    #   inherit realName getmail maildir;
    # };
  };
}
