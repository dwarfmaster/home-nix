{ config, lib, pkgs, ... }:

let

  realName = "Luc Chabassier";

  maildir = { path = "dwarfmaster/Inbox"; };

  getmail = {
    enable = true;
    delete = true;
    readAll = true;
    mailboxes = [ "Inbox" "Sent" ];
  };

in {
  imports = [
    ./mua.nix
    ./afew.nix
  ];

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  home.packages = [ pkgs.getmail ];
  programs.notmuch = {
    enable = true;
    maildir.synchronizeFlags = true;
    new.tags = [ "new" ];
    search.excludeTags = [ "spam" ];
    hooks = {
      postNew = "${pkgs.afew}/bin/afew --tag --new";
      preNew = ''
        getmail --rcfile getmailens --rcfile getmailmailoo --rcfile getmaillsv
      '';
    };
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
      astroid.sendMailCommand = "${pkgs.msmtp}/bin/msmtp --read-envelope-from --read-recipients --account dwarfmaster";

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

    "ens" = {
      address = "luc.chabassier@ens.fr";
      aliases = [ "luc.chabassier@ens.psl.eu" "chabassi@clipper.ens.fr" "chabassi@clipper.ens.psl.eu" ];
      inherit realName getmail maildir;
      userName = "chabassi";
      passwordCommand = "pass school/ens/clipper";
      astroid.enable = true;
      astroid.sendMailCommand = "${pkgs.msmtp}/bin/msmtp --read-envelope-from --read-recipients --account ens";

      imap = {
        host = "clipper.ens.fr";
        port = 993;
        tls.enable = true;
      };

      msmtp.enable = true;
      smtp = {
        host = "clipper.ens.fr";
        port = 465;
        tls.enable = true;
      };
    };

    "gmail" = let gm = getmail // { mailboxes = [ "Inbox" ]; }; in rec {
      address = "luc.chabassier@gmail.com";
      inherit realName maildir;
      flavor = "gmail.com";
      userName = address;
      passwordCommand = "pass mail/gmail.com/luc.chabassier";
      astroid.enable = true;
      astroid.sendMailCommand = "${pkgs.msmtp}/bin/msmtp --read-envelope-from --read-recipients --account gmail";
      getmail = gm;

      imap = {
        host = "imap.gmail.com";
        port = 993;
        tls.enable = true;
      };

      msmtp.enable = true;
      smtp = {
        host = "smtp.gmail.com";
        port = 587;
        tls = {
          enable = true;
          useStartTls = true;
        };
      };
    };

    "mailoo" = rec {
      address = "luc.linux@mailoo.org";
      inherit realName getmail maildir;
      userName = address;
      passwordCommand = "pass mail/mailoo.org/luc.linux";
      astroid.enable = true;
      astroid.sendMailCommand = "${pkgs.msmtp}/bin/msmtp --read-envelope-from --read-recipients --account mailoo";

      imap = {
        host = "mail.mailo.com";
        port = 993;
        tls.enable = true;
      };

      msmtp.enable = true;
      smtp = {
        host = "mail.mailo.com";
        port = 465;
        tls.enable = true;
      };
    };

    "lsv" = {
      address = "luc.chabassier@lsv.fr";
      inherit realName maildir;
      userName = "chabassier";
      passwordCommand = "pass school/these/luc.chabassier@lsv.fr";
      astroid.enable = true;
      astroid.sendMailCommand = "${pkgs.msmtp}/bin/msmtp --read-envelope-from --read-recipients --account lsv";

      imap = {
        host = "imaps.lsv.ens-cachan.fr";
        port = 993;
        tls.enable = true;
      };

      msmtp.enable = true;
      smtp = {
        host = "smtps.lsv.ens-cachan.fr";
        port = 587;
        tls = {
          enable = true;
          useStartTls = true;
        };
      };

      getmail = {
        enable = true;
        delete = true;
        readAll = true;
        mailboxes = [ "Inbox" ];
      };
    };

    # "hotmail" = {
    #   address = "luc.toulouse@hotmail.fr";
    #   aliases = [ "luc.toulouse@outlook.com" ];
    #   inherit realName getmail maildir;
    # };
  };
}
