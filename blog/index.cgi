#!/usr/bin/perl -w

use POSIX;
use CGI();

(undef,undef,undef,$todaymday,$todaymonth,$todayyear,undef,undef,undef) = localtime(time);
$todayyear += 1900;
$todaymonth++;
$todaymonth = "0$todaymonth" if ($todaymonth < 9);

my $selectedyear = CGI::param('year') || '';
my $selectedmonth = CGI::param('month') || '';

opendir(DIR,'entries');
my @entries = grep { !/^\./ && !/CVS/ } readdir(DIR);
closedir(DIR);

if ($selectedmonth eq '') {
    @entries = reverse sort @entries;
    ($selectedyear,$selectedmonth) = split /-/,$entries[0];
}

my $calHTML = makeSidebar($selectedmonth,$selectedyear,@entries);

my $HTML;
$HTML .= '<tr><td><h1 style="color: black; font-family:arial,helvetica,sans-serif"><a href="index.cgi">RAYTRACER<i style="font-family:serif; font-weight:normal">Blog</i></a></h1></td></tr>';
$HTML .= '<tr><td>&nbsp;</td></tr>';
$HTML .= '<tr><td>&nbsp;</td></tr>';

my $i = 0;
my $selectedprefix = "$selectedyear-$selectedmonth";
foreach my $entry (sort {$b cmp $a} @entries) {
    next unless $entry =~ /^$selectedprefix/;
    my $date = $entry;
    $date =~ s/\.html//;
    my $entryHTML = getEntry($entry);
    $HTML .= qq|<tr><td style="border-bottom: 1px dotted #999999; border-top: 1px dotted #999999">$date</td></tr>|;
    $HTML .= qq|<tr><td>&nbsp;</td></tr>|;
    $HTML .= qq|<tr><td align="justify">$entryHTML</td></tr>|;
    $HTML .= qq|<tr><td>&nbsp;</td></tr>|;
}


print <<"EOF";
Content-type: text/html

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>RAYTRACER Blog</title>
<style type="text/css">
body {
      /* font-family: "Trebuchet MS",Georgia,Helvetica,Arial; */
       font-size: 12pt;
}

span.title {
       color: #202080;
    letter-spacing: 0.1em;
}

p {
       margin-top: 0px;
}

A 		{ color: #CC6633; text-decoration: none; } 
A:link		{ color: #CC6633; text-decoration: none; } 
A:visited	{ color: #CC6633; text-decoration: none; } 
A:active	{ color: #FF9966;  } 
A:hover		{ color: #FF9966;  } 

/*
a {
       text-decoration: none;
       color: #804080;
}

a:hover {
        color: #806080;
	text-decoration: underline;
}
*/

a img {
      border: 2px solid black;
}

table.cal {
       font-size: 9pt;
       text-align: center
}

table.cal th {
       color: #408080;
}

pre {
       color: #808040;
}

div.vim {
       background-color: #e0e0c0;
       border: 1px solid #808060;
       color: #202020;
       padding: 10px;
       font-family: LucidaTypewriter, Courier, sans-serif;
       font-size: 10pt;
       white-space: pre;
}

div.shell {
       background-color: #202060;
       border: 1px solid #8080c0;
       color: #c0c0c0;
       padding: 10px;
       font-family: LucidaTypewriter, Courier, sans-serif;
       font-size: 10pt;
       white-space: pre;
}

tt {
       color: #808040;
}

code {
       color: #808040;
}

span.title {
       color: #408080;
       font-family: Arial,Helvetica,Sans-serif;
       font-size: 1.1em;
}

blockquote {
       font-size: 0.9em;
}

acronym, abbr {
     border-bottom: thin dashed green;
}

ol.chinese li {
  list-style-type: cjk-ideographic;
  padding-bottom: 1em;
}

div.sidebar {
   position: fixed;
   left: 640px;
   top: 0px;
   background-color: #eae8e3;
   height: 100%;
   width: 230px;
   bottom: 0;
   padding: 4px;
   border-left: 1px #a0a0a0 dotted;
}

</style>
<link REL="Shortcut Icon" HREF="favicon.png">
</head>
<body>

<table width="100%"><tr>
<td valign="top" align="center">
<table width="400">
$HTML
</table>
</td>
<td valign="top">
$calHTML
</td>
</tr></table>

</body>
</html>
EOF


sub getEntry {
   my $f = shift;
   open FILE,"entries/$f";
   my $data;
   foreach (<FILE>) {
       s/<h>/<span class="title">/;
       s/<\/h>/<\/span>/;
       s/---/&mdash;/g;
       $data .= $_;
   }
   close FILE;
   return $data;
}

sub makeSidebar {
    my ($month,$year,@entries) = @_;
    my %ents;
    my $HTML = "";
    foreach my $name (@entries) {
	$name =~ s/-...html$//;
	$ents{$name} = 1;
    }
    foreach my $name (sort {$b cmp $a} keys %ents) {
	my ($y,$m) = split('-',$name);
	$name = "<b>$name</b>" if ($y == $year && $m == $month);
	$HTML .= qq|<a href="index.cgi?year=$y&amp;month=$m">$name</a><br>|;
    }
    return $HTML;
}

sub makeCal {
   my ($mon,$year) = @_;
   my @months = qw( x Jan Feb Mar Apr Maj Jun Jul Aug Sep Okt Nov Dec);
   my $month = $months[$mon];
   my $html = '<table class="cal">';
   $html .= qq|<tr><td colspan="7" style="font-size: 10pt; color: #804080">$month $year</td></tr>|;
   $html .= qq|<tr><th>Man</th><th>Tirs</th><th>Ons</th><th>Tors</th><th>Fre</th><th>Lør</th><th>Søn</th></tr>|;
   $html .= qq|<tr height="1"><td height="1" bgcolor="#808080" colspan="7"><img height="1" width="1" src="trans1x1.gif"></td></tr>|;
   my $time =  POSIX::mktime(0, 0, 12, 1, $mon-1, $year-1900, 0, 0, 0);
   $html .= '<tr>';
   my ($sec,$min,$hour,$mday,$m,$y,$wday,$yday,$isdst) = localtime($time);
   $html .= '<td></td>'x(($wday-1)%7);
   while ($m < $mon) {
       my $style='';
       if ($mon == $todaymonth && $mday == $todaymday && $year == $todayyear) {
	   $style = 'style="background-color: #c0c0c0"';
       }
       if (dateHasEntry($year,$mon,$mday)) {
           $mday = qq|<a href="index.cgi?year=$year&mon=$mon#$mday">$mday</a>|;
       }
       $html .= "<td $style>$mday</td>";
       $html .= '</tr><tr>' if ($wday-1)%7 == 6;
       $time += 24*3600;
       ($sec,$min,$hour,$mday,$m,$y,$wday,$yday,$isdst) = localtime($time);
   }
   $html .= '</tr>';
   $html .= qq|<tr height="1"><td height="1" bgcolor="#808080" colspan="7"><img height="1" width="1" src="trans1x1.gif"></td></tr>|;
   $html .= '</table>';
   return $html;
}

sub dateHasEntry {
    my ($year,$mon,$mday) = @_;
    $mon = "0$mon" if $mon < 10;
    $mday = "0$mday" if $mday < 10;
    return -e "entries/$year-$mon-$mday.html";

}
