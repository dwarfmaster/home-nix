{
  config,
  pkgs,
  lib,
  ...
}: {
  plugins.comment-nvim = {
    enable = true;
    sticky = true; # Avoid moving the cursor
  };
  plugins.which-key = {
    bindings = {
      n."g".subs = {
        "c" = {
          name = "Comments";
          bindings = {
            "c" = {description = "Toggle comment";};
            "o" = {description = "Insert comment next line";};
            "O" = {description = "Insert comment previous line";};
            "A" = {description = "Insert comment end of line";};
          };
        };
        "b".name = "Block comments";
        "b".bindings."c" = {description = "Toggle block comment";};
      };
      v."g".bindings = {
        "c" = {description = "Toggle comment";};
        "b" = {description = "Toggle block comment";};
      };
    };
    operators = {
      "gc" = "Comment";
      "gb" = "Block comment";
    };
  };
}
