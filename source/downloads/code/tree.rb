class Tree
  attr_accessor :children, :node_name
  
  def initialize(name, children=[])
    @children = children
    @name = name
  end

  def self.build(hash)
    children = h_to_children(hash)
    if children.length == 1
      children[0]
    else
      Tree.new("root", children)
    end
  end
  
  def visit_all(&block)
    visit &block
    children.each {|c| c.visit_all &block }
  end
  
  def visit(&block)
    block.call self
  end
  
  private 
    def self.h_to_children(hash = {})
      hash.collect {|k,v| Tree.new(k, Tree::h_to_children(v)) }
    end
end
