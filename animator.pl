#!/usr/bin/perl

my $FRAMES = 100.0;
my $INPUTSCENE = "scenes/animscene.gay";

foreach my $frame (0..($FRAMES-1)) {
    $angle = ($frame/$FRAMES)*90.0;
    print "Rendering $frame/$FRAMES\n";
    open(FILE,$INPUTSCENE);
    my @output;
    while (<FILE>) {
	s/ANGLE/$angle/;
	push @output,$_;
    }
    close(FILE);
    $framescene = "scenes/tmp.gay";
    open(FILE,">$framescene");
    foreach (@output) {
	print FILE $_;
    }
    close (FILE);
    $framepicture = "frames/frame$frame.png";
    system("./src/tracer $framescene out.tga");
    system("convert out.tga $framepicture");
    system("rm $framescene");
}
