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

def make_calendar(years, entries_by_month)
   html = ''
   years.each { |year|
     html += "#{year}<br>"
     12.downto(1) { |i|
        if (entries_by_month.member?("#{year}-#{i}")) 
           html += "<a href='#{year}/#{i}.html'>#{i}</a>&nbsp;"
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
    html = ''
    entries.each { |e|
      html += e.output_html
    }
    FileUtils.mkdir_p(outdir)
    layout = File.new("layout-monthly.html").read
    layout = layout.gsub('CONTENT', html)
    layout = layout.gsub('TITLE', "Raygay blog for #{date.year}/#{date.month}")
    layout = layout.gsub('CALENDAR', make_calendar(years, entries_by_month))
    File.new(outfile,"w+").write(layout)
end

FileUtils.mkdir_p(BUILD_DIR)
FileUtils.cp('style.css',BUILD_DIR)
FileUtils.cp('favicon.png',BUILD_DIR)
if (not File.exists?(BUILD_DIR+"/files")) 
  FileUtils.cp_r('files', BUILD_DIR, :preserve=>true)
end
entries_by_month = Hash.new

entry_filenames = Dir.glob("entries/*.html")
all_years = Hash.new

entry_filenames.each { |f|
    e = Entry.new(f)
    key = "#{e.date.year}-#{e.date.month}"
    if entries_by_month.member?(key)
      entries_by_month[key].push(e)
    else
      entries_by_month[key] = [e]
    end
    all_years[e.date.year] = 1
}

entries_by_month.each_key { |key|
  make_month(entries_by_month[key], all_years.keys.sort.reverse, entries_by_month)
}

