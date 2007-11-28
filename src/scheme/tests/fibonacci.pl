sub fib {
    my $n = shift;
    if ($n < 2) {
        return $n
    } else {
        return fib($n-1) + fib($n-2);
    }
}

foreach my $i (0..35) {
    print "$i => ".fib($i)."\n";
}


