#!/usr/bin/perl

while (<>) {
    ($a,$b,$c) = /(\d+) (\d+) (\d+)/;
    print "".($a-1)." ".($b-1)." ".($c-1)."\n";
}
