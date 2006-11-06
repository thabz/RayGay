#!/usr/bin/perl

# Converts a .ply file to a .scm mesh
#
# Format is described at:
# http://astronomy.swin.edu.au/~pbourke/geomformats/ply/
#
# Download objects at:
# http://graphics.stanford.edu/data/3Dscanrep/

open(INPUT,$ARGV[0]);
open(OUTPUT,">".$ARGV[1]);

my $vertices;
my $faces;

# Check input format

if (<INPUT> !~ /ply/) {
    print "Inputfile is not a ply-object.\n";
    exit;
}

# Read header
my $done = 0;
while ($done == 0) {
   $line = <INPUT>;
   if ($line =~ /element vertex (\d*)/) {
       $vertices = $1;
   }
   if ($line =~ /element face (\d*)/) {
       $faces = $1;
   }
   $done = 1 if ($line =~ /^end_header/);
}

print "Vertices: $vertices\n";
print "Faces: $faces \n";

# Print gay header
print OUTPUT "(make-mesh \n";

# Convert vertices
print OUTPUT "   '(\n";
foreach my $i (1..$vertices) {
    $line = <INPUT>;
    $line =~ s/^\s*//;
    $line =~ s/\s*$//;
    $line =~ s/,/ /g; 
    print OUTPUT "      ($line)\n";
}
print OUTPUT "   )\n"; # end of vertices

# Convert faces
print OUTPUT "   '(\n";
foreach my $i (1..$faces) {
    $line = <INPUT>;
    $line =~ s/^\s*//;
    $line =~ s/\s*$//;
    if ($line !~ /3 /) {
	print "Face is not a triangle. Aborting.";
	exit;
    }
    $line =~ s/3 //;
    $line =~ s/,/ /g; 
    print OUTPUT "      ($line)\n";
}
print OUTPUT "   )\n"; # end of faces

print OUTPUT ")\n"; # end of mesh
