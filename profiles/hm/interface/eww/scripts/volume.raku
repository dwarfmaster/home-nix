
my $proc = Proc::Async.new(:r, 'pactl', 'subscribe');
my $sem = Semaphore.new(1);
my $count = 0;
my $out = $*OUT;

sub print_state ($show) {
  my $volume = qqx{pamixer --get-volume}.chomp();
  my $muted = qqx{pamixer --get-mute}.chomp();
  my $rshow = $show && ($muted ~~ "false");
  my $toshow = $rshow ??  "true" !! "false";
  $out.put('{ "volume": ', $volume, ', "muted": ', $muted, ', "show": ', $toshow, ' }');
  $out.flush
}

sub setup_promise {
  $sem.acquire();
  $count++;
  $sem.release();
  Promise.in(2).then({ 
    $sem.acquire();
    $count--;
    print_state False if $count == 0;
    $sem.release();
  })
}

print_state(False);
react {
  whenever $proc.stdout.lines {
    if $_ ~~ /sink/ {
      print_state True;
      setup_promise;
    }
  }
  whenever $proc.stderr {
    say "Error: ", $_;
  }
  whenever $proc.start {}
}
