#!/usr/bin/env ruby

require 'date';
require 'cgi';

#
# getEntry(String filename) --------------------------------------
#

def getEntryAsHTML(filename)
   result = ""
   File.new(filename).each_line { |line| 
      line.sub!(/<h>/,'<span class="title">')
      line.sub!(/<\/h>/,'</span>')
      line.sub!('---','&mdash;')
      result += line;
   }
   return result
end

#
# getAgenda(year,month)
#

def getAgendaAsHTML(year,month)
  result = "";
  file = sprintf("%4d-%02d.html",year,month)
  if Dir.entries("agendas").include?(file)
     File.new("agendas/"+file).each_line {|line|
       result += line+"<br>"
     }
  end
  return result;
end  

#
# entryExists(filename) ------------------------------------------
#

def entryExists(filename)
  return Dir.entries("entries").include?(filename+".html")
end 

#
# makeCalHTML(year,mon) ------------------------------------------
#

def makeCalHTML(year,monBegin,selectDay)
   months = ['Jan','Feb','Mar','Apr','Maj','Jun',
             'Jul','Aug','Sep','Okt','Nov','Dec'];

   pMon = monBegin == 1 ? 12 : monBegin-1;
   pYear = monBegin == 1 ? year - 1 : year;
   nMon = monBegin == 12 ? 1 : monBegin+1;
   nYear = monBegin == 12 ? year + 1 : year;
   prevMon = "<a href='index.cgi?year=#{pYear}&mon=#{pMon}'>&lt;</a>";
   nextMon = "<a href='index.cgi?year=#{nYear}&mon=#{nMon}'>&gt;</a>";

result = <<"EOF"
<table class="cal">
<tr><td>#{prevMon}</td><td colspan="5" style="font-family: Arial, Helvetica; font-size: 10pt; color: #804080">#{months[monBegin-1]} #{year}</td><td>#{nextMon}</td></tr>
<tr><th>Man</th><th>Tirs</th><th>Ons</th><th>Tors</th><th>Fre</th><th>Lør</th><th>Søn</th></tr>
<tr height="1"><td height="1" bgcolor="#808080" colspan="7"><img height="1" width="1" src="trans1x1.gif"></td></tr>
EOF

d = Date.new(year,monBegin,1);
now = Date.today;

result += '<td></td>'*((d.cwday-1)%7)

while d.month == monBegin 
   style = 'style="color: #a0a0a8"';
   if now.year == d.year && now.month == d.month && now.mday == d.mday
      style = 'style="background-color: #c0c0c0;"';
   end
   if d.mday == selectDay && d.month == monBegin
      style = 'style="border: 1px solid #c0c0c0;"';
   end
   link = entryExists(d.to_s) ? "<a href='index.cgi?year=#{d.year}&mon=#{d.month}&day=#{d.mday}'>#{d.mday}</a>" : d.mday
   result += "<td #{style}>#{link}</td>";
   result += "</tr><tr>" if d.cwday % 7 == 0;
   d = d + 1;
end

result += <<"EOF"
   </tr>
   <tr height="1"><td height="1" bgcolor="#808080" colspan="7">
   <img height="1" width="1" src="trans1x1.gif"></td></tr></table>
EOF

return result;
end


#
# main -----------------------------------------------------------
#

cgi = CGI.new
year = cgi['year'].empty? ? Date.today.year : cgi['year'].first.to_i 
monBegin = cgi['mon'].empty? ? Date.today.month : cgi['mon'].first.to_i 
mday = cgi['day'].empty? ? Date.today.mday : cgi['day'].first.to_i 
calHTML = makeCalHTML(year,monBegin,mday)
#calHTML = ''

agendaHTML = getAgendaAsHTML(year,monBegin)

weekdays = ['','Man','Tirs','Ons','Tors','Fre','Lør','Søn']

html = ""
entries = Dir.entries("entries").sort.reverse
offset = 0
unless cgi['day'].empty?
    offset = entries.index(sprintf("%4d-%02d-%02d.html",cgi['year'].first,cgi['mon'].first,cgi['day'].first))
end

entries[offset..offset+13].each { |f| 
  unless f =~ /^\./
    date = f.sub(/.html$/,'')
    (x,y,m,d) = /^(....)\-(..)\-(..)$/.match(date).to_a
    date = weekdays[Date.new(y.to_i,m.to_i,d.to_i).cwday] + "&nbsp;" + date
    
    entry = getEntryAsHTML("entries/"+f)
    html += <<"EOF"
    <tr><td width="10" nowrap>#{date}</td>
    <td><hr noshade size=1 color="black" style="color:black">
    </td></tr>
    <tr><td colspan="2" align="justify">#{entry}</td></tr>
    <tr><td width="10">&nbsp;</td><td></td></tr>
EOF
    end
}



puts <<"EOF"
Content-type: text/html

<html>
<head>
<title>Blog</title>
<link REL="Shortcut Icon" HREF="favicon.png">
<style>
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
       font-family: LucidaTypewriter;       		
       font-size: 10pt;
       white-space: pre;
}

div.shell {
       background-color: #202060;
       border: 1px solid #8080c0;
       color: #c0c0c0;
       padding: 10px;
       font-family: LucidaTypewriter;       		
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
       font-family: Arial,Helvetica,Sans;
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
   align: center;
}
</style>
</head>
<body>
<table width="100%"><tr>
<td width="100">&nbsp;</td>
<td valign="top">
<table width="400">
#{html}
</table>
</td>
<td valign="top">
</td>
</tr></table>

<div class="sidebar">
#{calHTML}
<small>
#{agendaHTML}
<small>
</div>

</body>
</html>
EOF


