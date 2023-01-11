
my $proc = Proc::Async.new(:r, '{{{inotifywait}}}', '-m', '-e', 'CLOSE_WRITE',
                               '{{{backlight_device}}}/brightness');
my $bctl = "{{{bctl}}}";
my $sem = Semaphore.new(1);
my $count = 0;
my $out = $*OUT;

sub print_state ($show) {
  my $light= qqx{$bctl -m | cut -d, -f 4 | tr -d %}.chomp();
  my $toshow = $show ??  "true" !! "false";
  $out.put('{ "light": ', $light, ', "show": ', $toshow, ' }');
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
    print_state True;
    setup_promise;
  }
  whenever $proc.stderr {
  }
  whenever $proc.start {}
}
