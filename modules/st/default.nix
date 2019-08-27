{ pkgs, ... }:

with pkgs; {
  packages = [(
    st.override { conf = builtins.readFile ./st.h; }
  )];
}

