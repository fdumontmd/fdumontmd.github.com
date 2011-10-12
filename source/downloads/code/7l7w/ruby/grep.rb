#!/usr/bin/env ruby

def grep(t, f)
  f.each_line.with_index {|l, i| print "#{i+1}: #{l}" if l.index(t) }
end

grep(ARGV.shift, ARGF.read)



