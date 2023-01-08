rm -rf $2 || true
mkdir -p $2
dir=$(realpath $1)
find "$dir" -type f -print0 | while IFS= read -r -d $'\0' file; do
  rel=$(realpath --relative-to="$dir" "$file")
  reldir=$(dirname "$rel")
  mkdir -p "$2/$reldir"
  case $rel in
    *.yuck)
      ;&
    *.scss)
      {{{mustache}}} {{{json}}} $file > "$2/$rel"
      ;;
    *.json)
      json="$2/$rel"
      {{{mustache}}} {{{json}}} $file > $json
      svg="$2/${rel%.json}.svg"
      png="$2/${rel%.json}.png"
      width=$(cat $json | {{{jq}}} .width)
      height=$(cat $json | {{{jq}}} .height)
      cat << EOF > "$svg"
<svg xmlns="http://www.w3.org/2000/svg"
     height="$(cat $json | {{{jq}}} .path.height)"
     width="$(cat $json | {{{jq}}} .path.width)">
  <path stroke=$(cat $json | {{{jq}}} .color) 
        fill=$(cat $json | {{{jq}}} .color)
        d=$(cat $json | {{{jq}}} .path.content)/>
</svg>
EOF
      {{{inkscape}}} -w $width -h $height $svg -o $png
      rm -f $json $svg
      ;;
  esac
done
