
my $nmcli = "{{{nmcli}}}";
my $jq = "{{{jq}}}";
my $device = "{{{wifi_device}}}";
my $proc = Proc::Async.new(:r, $nmcli, 'monitor');
my $out = $*OUT;

sub print_state {
  my $wifi = qqx{$nmcli d show $device | grep GENERAL.CONNECTION | cut -d: -f2 | xargs}.chomp();
  my $hard = qqx{rfkill -J --output TYPE,HARD | $jq '.rfkilldevices[] | select(.type=="wlan") | .hard'}.chomp();
  my $blocked = $hard eq "blocked" ?? "true" !! "false";
  my $connected = $wifi eq "--" ?? "false" !! "true";
  $out.put('{ "wifi": "', $wifi, '", "connected": ', $connected, ', "blocked": ', $blocked, ' }');
  $out.flush
}

print_state;
react {
  whenever $proc.stdout.lines {
    if ($_ ~~ "$device: connected" 
        || $_ ~~ "$device: disconnected") {
      print_state;
    }
  }
  whenever $proc.stderr {
  }
  whenever $proc.start {}
}
