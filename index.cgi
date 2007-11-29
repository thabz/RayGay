#!/usr/bin/perl -w

use POSIX;
use CGI();

my @months = qw( x Jan Feb Mar Apr Maj Jun Jul Aug Sep Okt Nov Dec);

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
$HTML .= '<tr><td><h1 style="color: black; font-family:arial,helvetica,sans-serif"><a href="index.cgi"><i style="font-family:serif; font-weight:normal">Blog</i></a></h1></td></tr>';
$HTML .= '<tr><td>&nbsp;</td></tr>';
$HTML .= '<tr><td>&nbsp;</td></tr>';

my $i = 0;
my $selectedprefix = "$selectedyear-$selectedmonth";
foreach my $entry (sort {$b cmp $a} @entries) {
    next unless $entry =~ /^$selectedprefix/;
    my $date = $entry;
    $date =~ s/\.html//;
    my $entryHTML = getEntry($entry);
    $HTML .= qq|<tr><td class="dateheader">$date</td></tr>|;
    $HTML .= qq|<tr><td>&nbsp;</td></tr>|;
    $HTML .= qq|<tr><td align="justify">$entryHTML</td></tr>|;
    $HTML .= qq|<tr><td>&nbsp;</td></tr>|;
}


print <<"EOF";
Content-type: text/html; charset=ISO-8859-1

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>Blog</title>
<link rel="alternate" type="application/rss+xml" title="RayGay blog" href="http://jesper.kalliope.org/blog/feed.cgi">
<script src="prototype.js"></script>
<script src="effects.js"></script>
<style type="text/css">
body {
      /* font-family: "Trebuchet MS",Georgia,Helvetica,Arial; */
      font-size: 12pt;
      height: 100%;
      padding: 0px;
      margin: 0px 0px 0px -140px;
}

td.dateheader {
    font-family: Arial,Helvetica, Sans;
    border-bottom: 1px dashed #999999; 
    background-color: #f8f8f8;
    border-top: 1px dashed #999999;
    padding: 2px 2px 2px 2px;
    font-weight: bold;
    color: #808080;
    text-align: right;
    letter-spacing: 2pt;
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
A:visited	{ color: #808080; text-decoration: none; } 
A:active	{ color: #FF9966;  } 
A:hover		{ color: #FF9966; text-decoration: underline; } 

/*
a {
       text-decoration: none;
       color: #804080;
}


a img {
      border: 2px solid black;
}
*/

img {
    border: 0px;        
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
       -moz-border-radius: 6.5px;
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
       font-family: Helvetica,Sans-serif;
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
   top: 0px;
   right: 0px;	
   height: 100%;
   width: 140px;
   float: none;
   position: fixed;	  
   background-color: #eae8e3;
   background-image: url(logo.png);
   background-repeat: no-repeat;
   background-position: bottom center;
   padding: 4px 0px 0px 20px;
   border-left: 1px solid #c0c0c0;
   overflow: hidden;
   line-height: 150%;
   font-family: Helvetica, sans;
}

div.sidebar a {
   color: #808080;
   letter-spacing: 2pt;
}

</style>
<script>

function colorize() {
    divs = document.getElementsByTagName("div");
    var ks = new Array("let\\*","let","list","define","append",
        "begin","if","do","cond","case","else","display", "newline",
        "magnitude","\\+","\\-","\\*","\\/");
    for(i = 0; i < divs.length; i++) {
	    mydiv = divs[i];
	    if (mydiv.id == 'scheme') {
	        html = mydiv.innerHTML;
/*	        html = html.replace(/^;(.*)\\\n/g,'<font color="#4040a0">;\\\$1</font>\\n'); */
	        html = html.replace(/([\\\( ]+)(-?[0-9]+\\\.?[0-9]*)/g,'\\\$1<font color="#a040a0">\\\$2</font>');
	        html = html.replace(/(#[ft])/g,'\\\<font color="#a040a0">\\\$1</font>');
	        for(var j = 0; j < ks.length; j++) {
	            var k = ks[j];
                var re = new RegExp("\\\\("+k,"g");
	            html.replace(re,"<font color='#a04040'>(<b>"+k+"</b></font>")
	        };
	        html = html.replace(/\\\(/g,'<font color="#4040a0">(</font>');
	        html = html.replace(/\\\)/g,'<font color="#4040a0">)</font>');
	        mydiv.innerHTML = html;
	    }
    }
}
</script>
<link REL="Shortcut Icon" HREF="favicon.png">
</head>
<body onload="colorize()">
<div class="sidebar">
$calHTML
</div>
<table width="100%"><tr>
<td valign="top" align="center">
<table width="400">
$HTML
</table>
</td>
<td valign="top">
<!-- $calHTML -->
</td>
</tr></table>
<script src="http://www.google-analytics.com/urchin.js" type="text/javascript">
</script>
<script type="text/javascript">
_uacct = "UA-2135385-2";
urchinTracker();
</script>
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
	$ents{$name} += 1;
    }
    my $prev_y = '';
    foreach my $name (sort {$b cmp $a} keys %ents) {
	my ($y,$m) = split('-',$name);
	if ($y ne $prev_y) {
            my $icon = ''; #$y == $year ? '&#9662;' : '&#9656;';       
	    $HTML .= qq|<a href="javascript:{}" onclick="Element.toggle('year_$y')">$icon <b>$y</b></a><br>|;
	    my $display = $y eq $year ? 'block' : 'none';
    	    $HTML .= qq|<div id="year_$y" style="display:$display">|;        
	    $prev_y = $y;
            foreach my $name (sort {$b cmp $a} keys %ents) {
            	my ($y,$m) = split('-',$name);
            	if ($y eq $prev_y) {
              	    my $count = $ents{$name};
        	    $name = $months[$m];
        	    $name = "<b>$name</b>" if ($y == $year && $m == $month);
        	    $title = $count == 1 ? "1 entry" : "$count entries";
        	    $HTML .= qq|&nbsp;&nbsp;&nbsp;&nbsp;<a title="$title" href="index.cgi?year=$y&amp;month=$m">$name</a><br>|;
            	}
            }	            
	    $HTML .= '</div>'    
	}
    }
    
    $HTML .= '<br><br><a href="library.cgi">Bibliotek</a>';
    return $HTML;
}

sub makeCal {
   my ($mon,$year) = @_;
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
