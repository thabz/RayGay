#!/usr/bin/perl

use utf8;

my $DEST_DIR = "_posts";
my $SRC_DIR = "old-posts";

opendir(DIR, $SRC_DIR);
my @entries = grep { !/^\./ && !/CVS/ } readdir(DIR);
closedir(DIR);

foreach my $entry (@entries) {
    my $date = $entry;
    $date =~ s/\.html//;
    my ($year,$mon,$mday) = split "-",$date;

    my $descr = getEntry($entry);
    $title = "";
    my $descr2 = $descr;
    while ($descr2 =~ s/<h>(.*?)<\/h>//mi) {
	$title .= $1." - ";
    }
    $title =~ s/ - $//;
    $title =~ s/&pi;/π/;
    $title =~ s/&oslash;/ø/;
    $descr =~ s!href="files/!href="{{site.url}}/assets/!g;
    $descr =~ s!src="files/!src="{{site.url}}/assets/!g;
    $descr =~ s!<h>.*</h>!!;
    $descr =~ s!<h>(.*)</h>!<h2>\1</h2>!g;

    writeEntry($title,$date,$descr);
}

sub getEntry {
   my $f = shift;
   open FILE, "< :encoding(Latin1)", "$SRC_DIR/$f";
   my $data;
   foreach (<FILE>) {
       s/---/&mdash;/g;
       $data .= $_;
   }
   close FILE;
   return $data;
}

sub writeEntry {
    my ($title,$date,$body) = @_; 
    my $titlehyphenated = $title;
    $titlehyphenated =~ s/ - /-/g;
    $titlehyphenated =~ s/ /-/g;
    $titlehyphenated = lc($titlehyphenated);
    open FILE, "> :encoding(UTF-8)", "$DEST_DIR/$date-$titlehyphenated.html";
    print FILE "---\n";
    print FILE "layout: post\n";
    print FILE "title:  $title\n";
    print FILE "date:   $date\n";
    print FILE "categories: raygay update\n";
    print FILE "---\n";
    print FILE $body;
    close FILE;
}
