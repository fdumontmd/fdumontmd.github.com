module Patch
  def self.included(klass)
    puts klass.instance_methods.member?(:my_attribute)
  end
end


class Target1
  print "In Target1: "
  include Patch
  attr_accessor :my_attribute
end

class Target2
  print "In Target2: "
  attr_accessor :my_attribute
  include Patch
end