#!/usr/bin/ruby -w

require 'date'
require 'fileutils'

BUILD_DIR = 'build'

class Entry
    attr_accessor :title, :date, :content, :filename
		   
    def initialize(filename)
      parts = filename.scan(/\d+/)
      @date = Date.civil(parts[0].to_i,parts[1].to_i,parts[2].to_i)
      @filename = filename
    end
    
    def outdir
      "#{BUILD_DIR}/#{date.year}/#{date.month}"
    end
    
    def outfile
      "#{outdir}/#{date.day}.html"
    end
    
    def mtime
      File.mtime(filename)
    end
    
    def read_input_html
      # TODO: Support for Markdown files also
      if content.nil?
        content = File.new(filename).read
        content = content.gsub('<h>','<span class="title">')
        content = content.gsub('</h>','</span>')
        content = content.gsub('---','&mdash;')
      end
      return content
    end

    def output_html
      my_content = "<tr><td class='dateheader'>#{date.year}/#{date.month}/#{date.day}</td></tr>"
      my_content += '<tr><td>&nbsp;</td></tr>'
      my_content += "<tr><td align='justify'>#{read_input_html}</td></tr>"
      my_content += '<tr><td>&nbsp;</td></tr>'
    end
    
    def write_day_file
      FileUtils.mkdir_p(outdir)
      layout = File.new("layout-daily.html").read
      layout = layout.gsub('CONTENT', output_html)
      layout = layout.gsub('TITLE', "Raygay blog for #{date.year}/#{date.month}/#{date.day}")
      File.new(outfile,"w+").write(layout)
    end
end

def make_calendar(date, years, entries_by_month)
   html = ''
   years.each { |year|
     html += date.year == year ? "<b>#{year}</b>" : "#{year}"
     html += "<br>"
     12.downto(1) { |i|
        if (entries_by_month.member?("#{year}-#{i}")) 
          link_text = (date.month == i and date.year == year) ? "<b>#{i}</b>" : "#{i}"
          html += "<a href='#{year}/#{i}.html'>#{link_text}</a>&nbsp;"
        end
     }
     html += "<br>"
   }
   return html
end

def make_month(entries, years, entries_by_month)
  date = entries[0].date
  outdir =  "#{BUILD_DIR}/#{date.year}"
  outfile = "#{outdir}/#{date.month}.html"
  if (not FileUtils.uptodate?(outfile, entries.map { |e| e.filename }))
    html = '<tr><td><h1 style="color: black; font-family:arial,helvetica,sans-serif"><a href="index.html"><i style="font-family:serif; font-weight:normal">Blog</i></a></h1></td></tr>'
    html += '<tr><td>&nbsp;</td></tr>'
    html += '<tr><td>&nbsp;</td></tr>'
    entries.each { |e|
      html += e.output_html
    }
    FileUtils.mkdir_p(outdir)
    layout = File.new("layout-monthly.html").read
    layout = layout.gsub('CONTENT', html)
    layout = layout.gsub('TITLE', "Raygay blog for #{date.year}/#{date.month}")
    layout = layout.gsub('CALENDAR', make_calendar(date,years, entries_by_month))
    puts "Outputting #{outfile}"
    File.new(outfile,"w+").write(layout)
  end
end

def latest_entry(entries)
  latest = entries[0]
  entries.each { |e|
    if e.date > latest.date
      latest = e
    end
  }
  latest
end

FileUtils.mkdir_p("#{BUILD_DIR}/files")
FileUtils.cp('style.css',BUILD_DIR)
FileUtils.cp('favicon.png',BUILD_DIR)
FileUtils.cp('logo.png',BUILD_DIR)
FileUtils.cp('prototype-1.6.0.3.js',BUILD_DIR)
FileUtils.cp(Dir.glob('files/*'), "#{BUILD_DIR}/files")
entries_by_month = Hash.new

entry_filenames = Dir.glob("entries/*.html")
all_years = Hash.new
all_entries = Array.new

entry_filenames.each { |f|
    e = Entry.new(f)
    key = "#{e.date.year}-#{e.date.month}"
    if entries_by_month.member?(key)
      entries_by_month[key].push(e)
    else
      entries_by_month[key] = [e]
    end
    all_years[e.date.year] = 1
    all_entries.push e
}

entries_by_month.each_key { |key|
  make_month(entries_by_month[key], all_years.keys.sort.reverse, entries_by_month)
}

l = latest_entry(all_entries)
FileUtils.cp("#{BUILD_DIR}/#{l.date.year}/#{l.date.month}.html", "#{BUILD_DIR}/index.html")
