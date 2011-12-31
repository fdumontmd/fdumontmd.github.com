#!/usr/bin/env ruby

%w{rubygems neo4j}.each{|r| require r}

class Movie
  include Neo4j::NodeMixin
  property :name
  index :name
end

class Actor
  include Neo4j::NodeMixin
  property :name
  index :name
  has_n(:acted_in).to(Movie)
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

puts "begin processing..."

Neo4j::Config[:storage_path] = "#{ENV['NEO4J_HOME']}/data/graph.db"
count = 0

inserter = Neo4j::Batch::Inserter.new

File.open(ARGV[0]).each do |line|
  _, _, actor, movie = line.split("\t")
  next if actor.empty? || movie.empty?

  actor_node = get_or_create_node(inserter, actor, Actor)
  movie_node = get_or_create_node(inserter, movie, Movie)

  inserter.create_rel(Actor.acted_in, actor_node, movie_node)

  puts "  #{count} relationships loaded" if (count += 1) % 100 == 0
end

puts "done!"
inserter.shutdown
