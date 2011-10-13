module ActsAsCsv
  def self.included(base)
    base.extend ClassMethod
  end

  module ClassMethod
    def acts_as_csv
      include InstanceMethods
    end
  end

  module InstanceMethods
    def read
      @csv_contents = []
      filename = self.class.to_s.downcase + '.txt'
      file = File.new(filename)
      @headers = file.gets.chomp.split(', ')

      file.each do |row|
        @csv_contents << row.chomp.split(', ')
      end
    end

    attr_accessor :headers, :csv_contents

    def each(&block)
      @csv_contents.each {|r| block.call(CsvRow.new(@headers, r)) }
    end

    def initialize 
      read
    end
  end
end

class CsvRow
  def initialize(h, r)
    @headers = h
    @row = r
    singleton = class << self; self; end
    h.each do |field|
      singleton.send(:define_method,field.to_sym) { r[h.index(field)] }
    end
  end
end

class RubyCsv
  include ActsAsCsv
  acts_as_csv
end

