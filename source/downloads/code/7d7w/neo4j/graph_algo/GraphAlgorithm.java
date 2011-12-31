package jp.wakatta;

import java.util.Map;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.kernel.EmbeddedGraphDatabase;

import com.tinkerpop.blueprints.pgm.Edge;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.pgm.oupls.jung.GraphJung;

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath;
import edu.uci.ics.jung.graph.Graph;


public class GraphAlgorithm {
	public static void main(String...args) {
		String neo4jHome = "/users/x/neo4j-test";
		
		if (args.length > 0)
			neo4jHome = args[0];
		
		GraphDatabaseService graphDb = new EmbeddedGraphDatabase(neo4jHome + "/data/graph.db/");
		
		com.tinkerpop.blueprints.pgm.Graph g = new Neo4jGraph(graphDb);
		
		Graph<Vertex, Edge> j = new GraphJung(g);
		
		DijkstraShortestPath<Vertex, Edge> dijkstra = new DijkstraShortestPath<Vertex, Edge>(j);
		
		Node trent = graphDb.index().forNodes("Person_exact").get("name", "Trent").next();
		Node dave = graphDb.index().forNodes("Person_exact").get("name", "Dave").next();
		
		System.out.println("Distance between Trent and Dave:" + dijkstra.getDistance(g.getVertex(trent.getId()), g.getVertex(dave.getId())));
		
		System.out.println("Distance between Trent and everybody:");
		for (Map.Entry<Vertex, Number> kv: dijkstra.getDistanceMap(g.getVertex(trent.getId())).entrySet()) {
			System.out.println(graphDb.getNodeById((Long) kv.getKey().getId()).getProperty("name") + " => " + kv.getValue());
		}
		g.shutdown();
		graphDb.shutdown();
	}	
}
