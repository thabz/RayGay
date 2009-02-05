#!/usr/bin/ruby -w

require 'date'

class Entry
    attr_accessor :title, :date, :content
		   
    def initialize(filename)
       parts = filename.scan(/\d+/)
       @date = Date.civil(parts[0].to_i,parts[1].to_i,parts[2].to_i)
    end
end

def make_month(date) 

end

entry_filenames = Dir.glob("entries/*.html")
entry_filenames.each { |f|
    e = Entry.new(f)
    puts e.date	
}

