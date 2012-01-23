package jp.wakatta;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.Pipeline;


public class ISBNLoader {
	public static final String REDIS_HOST = "127.0.0.1";
	public static final int REDIS_PORT = 6379;
	public static final int TIMEOUT = 5000;
	public static final int BATCH_SIZE = 1000;
	private final Jedis client;
	
	protected ISBNLoader() {
		client = new Jedis(REDIS_HOST, REDIS_PORT, TIMEOUT);
		client.flushAll();
	}
	
	public static void main(String...args) throws Exception {
		new ISBNLoader().load(args[0]);
	}
	
	protected void load(String fileName) throws Exception {
		BufferedReader in = new BufferedReader(new FileReader(fileName));
		
		String line;
		int count = 0;
		
		List<Pair> batch = new LinkedList<Pair>();
		
		while ((line = in.readLine()) != null) {
			count++;
			if (count == 1)
				continue;
			String[] tokens = line.split("\t");
			if (tokens.length < 4)
				continue;
			String isbn = tokens[0];
			String title = tokens[3];
			if (isbn.isEmpty() || title.isEmpty())
				continue;
			batch.add(new Pair(isbn, title));
			
			if (batch.size() == BATCH_SIZE)
				flush(batch);
		}
		
		flush(batch);
		
		client.disconnect();		
	}
	
	protected void flush(List<Pair> batch) throws Exception {
		Pipeline pipe = client.pipelined();
		
		for (Pair p : batch)
			pipe.set(p.key, p.value);
		pipe.sync();
		batch.clear();
	}
	
	static class Pair {
		String key;
		String value;
		
		Pair(String key, String value) {
			this.key = key;
			this.value = value;
		}
	}
}
