package jp.wakatta;

import java.io.BufferedReader;
import java.io.FileReader;

import redis.clients.jedis.Jedis;


public class ISBNLoader {
	public static final String REDIS_HOST = "127.0.0.1";
	public static final int REDIS_PORT = 6379;
	public static final int TIMEOUT = 5000;

	public static void main(String...args) throws Exception {
		BufferedReader in = new BufferedReader(new FileReader(args[0]));
		
		Jedis jedis = new Jedis(REDIS_HOST, REDIS_PORT, TIMEOUT);
		jedis.flushAll();
		
		String line;
		int count = 0;
		
		
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
			jedis.set(isbn, title);
		}
		
		jedis.disconnect();
	}
}
