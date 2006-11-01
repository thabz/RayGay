#!/usr/bin/perl -w

use POSIX;
use CGI();
use XML::Twig;

my $twig = new XML::Twig(keep_encoding => 1);
$twig->parsefile('library.xml');
my @entries;
foreach my $entry ($twig->root->children('entry')) {
    my $e;
    $e->{'id'} = $entry->{'att'}->{'id'};
    $e->{'title'} = $entry->first_child('title')->xml_string;
    $e->{'fulltitle'} = $entry->first_child('fulltitle')->xml_string;
    $e->{'abstract'} = $entry->first_child('abstract')->xml_string;
    $e->{'pdf'} = $entry->first_child('pdf')->text;
    if ($entry->first_child('www')) {
	$e->{'www'} = $entry->first_child('www')->text;
    }
    my @authors;
    foreach my $a ($entry->first_child('authors')->children('author')) {
	push @authors, $a->text;
    }
    $e->{'authors'} = \@authors;
    push @entries,$e;
}

$contentHTML = '<h1 style="color: black; font-family:arial,helvetica,sans-serif"><a href="index.cgi"><i style="font-family:serif; font-weight:normal">Library</i></a></h1>';

foreach my $e (@entries) {
    $contentHTML .= entry2html($e);
}

my $sidebarHTML = '<a href="index.cgi">Blog</a>';

print <<"EOF";
Content-type: text/html; charset=ISO-8859-1

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title>Library</title>
<style type="text/css">
body {
      /* font-family: "Trebuchet MS",Georgia,Helvetica,Arial; */
       font-size: 12pt;
       height: 100%;
      padding: 0px;
      margin: 0px 0px 0px -140px;
}

td.dateheader {
    font-family: Arial, Helvetica;
    border-bottom: 1px dashed #999999; 
    background-color: #f8f8f8;
    border-top: 1px dashed #999999;
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

*/

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
   font-family: Helvetica, Arial, sans;
}

div.sidebar a {
   color: #808080;
   letter-spacing: 2pt;
}

div.item {
   padding: 2px 2px 20px 2px;
}

div.details {
    padding: 2px;   
    margin: 0px 0px 0px 20px;
    font-size: 0.8em;
    text-align: justify;
}

span.authors {
    font-variant: small-caps;
}


</style>
<link REL="Shortcut Icon" HREF="favicon.png">
<script src="prototype.js"></script>
<script src="effects.js"></script>
<script>
function toggleDetails(id) {
    var elem = \$('details'+id);
    if (elem.style.display == 'none') {
	/* new Effect.BlindDown(elem); */
	new Effect.Appear(elem);
	\$('button'+id).src = 'icons/delete.png';    
    } else {
	/* new Effect.BlindUp(elem); */
	new Effect.Fade(elem);
	\$('button'+id).src = 'icons/add.png';    
    }
}
</script>
</head>
<body>
<div class="sidebar">
$sidebarHTML
</div>
<table width="100%"><tr>
<td valign="top" align="center">
<table width="600">
<tr><td>
$contentHTML
</td></tr>
</table>
</td>
<td valign="top">
</td>
</tr></table>
</body>
</html>
EOF

sub entry2html {
    my $e = shift;
    my $id = $e->{'id'};
    my $pdf  = $e->{'pdf'};
    my $www  = $e->{'www'};
    my $html = '<div class="item">';
    $html .= "<b>".$e->{'title'}."</b><br>";
    $html .= '<span class="authors">';
    foreach my $a (@{$e->{'authors'}}) {
	$html .= $a . ", ";
    }
    $html =~ s/, $//;
    $html .= '</span>';
    $html .= '<br>';
    if ($e->{'keywords'}) {
	$html .= "Keywords: ";
	foreach my $a (@{$e->{'keywords'}}) {
	    $html .= $a . ", ";
	}
	$html .= '<br>';
    }
    $html .= qq|<a href="javascript:{toggleDetails('$id')}" title="Toggle summary"><img id="button$id" border="0" src="icons/add.png"></a> |;
    $html .= qq|<a href="$pdf"><img border="0" src="icons/page_white_acrobat.png" title="Download PDF"></a> | if $pdf;
    $html .= qq|<a href="$www"><img border="0" src="icons/world.png" title="Goto homepage"></a> | if $www;
    $html .= qq|<div class="details" id="details$id" style="display:none">|;
    $html .= $e->{'abstract'};
    $html .= '</div>';
    $html .= '</div>';
    return $html;
}
