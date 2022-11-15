{ writeScriptBin, curl, ... }:

writeScriptBin "reupload" ''
  if test "$#" -ne 1; then
    echo "Usage: $0 path/to/pdf"
    exit 1
  fi

  ${curl}/bin/curl 'http://10.11.99.1/upload' -H 'Origin: http://10.11.99.1' -H 'Accept: */*' \
    -H 'Referrer: http://10.11.99.1/' -H 'Connection: keep-alive'                             \
    -F "file=@$1;filename=$1;type=application/pdf"                                            \
    --progress-bar | tee
''
