#!/usr/bin/env ruby

%w{rubygems neo4j}.each{|r| require r}


class Person
  include Neo4j::NodeMixin
  property :name
  index :name
end

def get_or_create_node(inserter, name, clazz)
  n = inserter.index_get('name', name, :exact, clazz)
  n = n.first if n
  unless n
    n = inserter.create_node({'name' => name}, clazz)
    inserter.index_flush(clazz)
  end
  n
end

family = {
  'Alice' => {:sibbling_of => ['Bob', 'Carol'], :married_to => ['Walter'],:child_of => ['Trent', 'Peggy'] },
  'Bob' => {:sibbling_of => ['Alice', 'Carol'], :married_to => ['Eve'], :child_of => ['Trent', 'Peggy']},
  'Carol' => {:sibbling_of => ['Bob', 'Alice'], :child_od => ['Trent', 'Peggy'] },
  'Trent' => {:married_to => ['Peggy'], :parent_of => ['Alice', 'Bob', 'Carol']},
  'Peggy' => {:married_to => ['Trent'], :parent_of => ['Alice', 'Bob', 'Carol']},
  'Eve' => {:married_to => ['Bob']},
  'Walter' => {:married_to => ['Alice'], :sibling_of => ['Dave']},
  'Dave' => {:sibling_of => ['Walter']}
}

puts "begin processing..."

Neo4j::Config[:storage_path] = "#{ENV['NEO4J_HOME']}/data/graph.db"

inserter = Neo4j::Batch::Inserter.new

family.each do |k, _|
  inserter.create_node({'name' => k}, Person)
end

inserter.index_flush(Person)

family.each do |k, v|
  v.each do |r, os|
    os.each do |o|
      inserter.create_rel(r,
                          inserter.index_get('name', k, :exact, Person).next,
                          inserter.index_get('name', o, :exact, Person).next
                          )
    end
  end
end

puts "done!"
inserter.shutdown
