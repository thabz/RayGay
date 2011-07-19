#!/usr/bin/ruby

DICT='/usr/share/dict/words'

# Prints all anagrams that can be made from a word-list
# Jesper Christensen, 2011-07-18

$h = Hash.new
$biggest_num = 0
$biggest_key = 0

File.open(DICT).each{|line|
    line = line.strip
    key = line.scan(/./).sort.join('')
    if $h[key]
	$h[key].push(line)
    else
	$h[key] = Array[line]
    end	
}

$h.each{ |key,list|
    if list.size() > 1
#	puts list.join(', ')
    end
    if (list.size() > $biggest_num)
	$biggest_key = key
	$biggest_num = list.size()
    end
}

puts "Most anagrams: #{$h[$biggest_key].join(', ')}"
