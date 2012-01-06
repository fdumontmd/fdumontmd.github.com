package jp.wakatta;

import java.util.Iterator;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.server.plugins.Description;
import org.neo4j.server.plugins.Name;
import org.neo4j.server.plugins.PluginTarget;
import org.neo4j.server.plugins.ServerPlugin;
import org.neo4j.server.plugins.Source;

@Description("An extension to list all node names")
public class ListNames extends ServerPlugin {
	@Name("list_all_names")
	@Description("List all the node names")
	@PluginTarget(GraphDatabaseService.class)
	public Iterable<String> getAllNames(@Source GraphDatabaseService graphDb) {
		final Iterator<Node> nodeIterator = graphDb.getAllNodes().iterator();
		return new Iterable<String>() {
			public Iterator<String> iterator() {
				return new Iterator<String>() {
					@Override
					public void remove() { // do nothing 
					}
					@Override
					public String next() {
						try {
							return (String) nodeIterator.next().getProperty("name");
						} catch (Exception ex) {
							return "";
						}
					}
					@Override
					public boolean hasNext() {
						return nodeIterator.hasNext();
					}
				};
			}
		};
	}
}
