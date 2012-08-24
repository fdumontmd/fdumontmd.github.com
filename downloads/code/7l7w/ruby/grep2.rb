#!/usr/bin/env ruby

def grep(header,text, file)
  file.each_line.with_index {|line, index| print "#{header}#{index+1}: #{line}" if line.index(text) }
end

def make_header(filename, required)
  required ? "#{filename} - " : ""
end

text = ARGV.shift
header = ARGV.length > 1

grep("", text, ARGF) unless ARGV.length > 0

ARGV.each do |filename|
  begin
    File.open(filename) { |f| grep(make_header(filename, header), text, f) }
  rescue Exception
    puts "Error reading file #{filename}"
  end
end



