{ config, lib, pkgs, ... }:

{
  programs.beets = {
    enable = true;
    package = (pkgs.beets.override {
      keyfinder-cli = pkgs.keyfinder-cli;
      bs1770gain = pkgs.bs1770gain;
      flac = pkgs.flac;
      mp3val = pkgs.mp3val;
      enableAlternatives = true;
    });
    settings = {
      directory = "/data/luc/annex/music/files";
      library = "${config.xdg.cacheHome}/beets/database.db";
      ignore_hidden = true;
      asciify_paths = true;
      art_filename = "cover";
      threaded = true;
      format_item = "[$genre] $album ($albumartist): $title";
      format_album = "[$genre] $albumartist - $album";
      sort_item = "genre+ artist+ album+ disc+ track+";
      sort_album = "genre+ albumartist+ album+";
      sort_case_insensitive = true;
      original_date = false;
      per_disc_numbering = false;

      plugins = [
        "fetchart"
        "lyrics"
        "lastgenre"
        "edit"
        "keyfinder"
      ];

      import = {
        write = true;
        move = true;
        resume = "ask";
        incremental = true;
        from_scratch = true;
        quiet_fallback = "skip";
        none_rec_action = "ask";
        log = "${config.xdg.cacheHome}/beets/import.log";
        languages = [ "en" "fr" ];
        group_albums = true;
        bell = true;
      };

      match = {
        strong_rec_thresh = 0.04;
        max_rec = {
          unmatched_tracks = "medium";
        };
        ingore_video_tracks = true;
      };

      paths = {
        default = "$genre/$albumartist/$album%aunique{}/$track - $title";
        singleton = "$genre/singles/$artist/$title";
        comp = "$genre/Compilations/$album%aunique{}/$track - $title";
      };

      ui = {
        color = true;
      };

      keyfinder = {
        auto = true;
        bin = "${pkgs.keyfinder-cli}/bin/keyfinder-cli";
        overwrite = false;
      };

      fetchart = {
        auto = true;
        cautious = false;
        minwidth = 128;
        maxwidth = 720;
        sources = [
          "filesystem"
          "coverart"
          "albumart"
          "itunes"
          "amazon"
          "wikipedia"
        ];
      };

      lastgenre = {
        auto = true;
        canonical = true;
        fallback = "unknown";
        force = true;
        prefer_specific = true;
      };

      lyrics = {
        auto = true;
      };
    };
  };
}
