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
getmail --rcfile getmailens --rcfile getmailmailoo --rcfile getmailens
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

      imap = {
        host = "clipper.ens.fr";
        port = 993;
        tls.enable = true;
      };
    };

    "gmail" = rec {
      address = "luc.chabassier@gmail.com";
      inherit realName getmail maildir;
      flavor = "gmail.com";
      userName = address;
      passwordCommand = "pass mail/gmail.com/luc.chabassier";

      imap = {
        host = "imap.gmail.com";
        port = 993;
        tls.enable = true;
      };

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
      imap = {
        host = "mail.net-c.com";
        port = 993;
        tls.enable = true;
      };
    };

    # "hotmail" = {
    #   address = "luc.toulouse@hotmail.fr";
    #   aliases = [ "luc.toulouse@outlook.com" ];
    #   inherit realName getmail maildir;
    # };
  };
}
