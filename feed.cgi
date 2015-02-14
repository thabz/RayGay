#!/usr/bin/perl -w

opendir(DIR,'entries');
my @entries = grep { !/^\./ && !/CVS/ } readdir(DIR);
closedir(DIR);

my $xml = '';
my $i = 0;
foreach my $entry (sort {$b cmp $a} @entries) {
    last if ($i++ > 10);
    my $date = $entry;
    $date =~ s/\.html//;
    my ($year,$mon,$mday) = split "-",$date;
    my $pubDate = RFC822($year,$mon,$mday);
    my $descr = getEntry($entry);
    $title = "";
    my $descr2 = $descr;
    while ($descr2 =~ s/<h>(.*?)<\/h>//mi) {
	$title .= $1." - ";
    }
    $title =~ s/ - $//;
    $descr =~ s!href="files/!href="http://jesper.kalliope.org/blog/files/!g;
    $descr =~ s!src="files/!src="http://jesper.kalliope.org/blog/files/!g;
    $descr =~ s!<h>.*</h>!!;
#    $descr =~ s/</&lt;/g;
#    $descr =~ s/>/&gt;/g;
#    $title =~ s/</&lt;/g;
#    $title =~ s/>/&gt;/g;
    
    my $link = "http://jesper.kalliope.org/blog/?year=$year&month=$mon#$mday";
    $xml .= qq|<item>\n|;
    $xml .= qq|  <title><![CDATA[$title]]></title>\n|;
    $xml .= qq|  <link><![CDATA[$link]]></link>\n|;
    $xml .= qq|  <description><![CDATA[$descr]]></description>\n|;
    $xml .= qq|  <pubDate><![CDATA[$pubDate]]></pubDate>\n|;
    $xml .= qq|</item>\n|;
}

sub RFC822 {
    # format 17 Oct 1982 16:30:25 +0100
    my ($year,$mon,$mday) = @_;
    $mon--;
    my @monthsRFC822 = qw (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
    return "$mday $monthsRFC822[$mon] $year 12:00:00 +0100";
}

sub getEntry {
   my $f = shift;
   open FILE,"entries/$f";
   my $data;
   foreach (<FILE>) {
       s/---/&mdash;/g;
       $data .= $_;
   }
   close FILE;
   return $data;
}

print "Content-Type: text/xml; charset=ISO-8859-1\n\n";
print <<"EOFAA"
<?xml version="1.0" encoding="ISO-8859-1"?>
<rss version="2.0">
  <channel>
     <title>RayGay blog</title>
     <link>http://jesper.kalliope.org/blog/</link>
     <language>da</language>
     $xml
  </channel>
</rss>
EOFAA

