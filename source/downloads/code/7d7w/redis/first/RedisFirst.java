package jp.wakatta;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.Transaction;

public class RedisFirst {
	public static void main(final String...args) throws Exception {
		// connect
		Jedis jedis = new Jedis("localhost");
		// set the key first to 5
		jedis.set("first", "5");
		
		// start a transaction
		Transaction trans = jedis.multi();
		// increase by 4
		trans.incrBy("first", 4);
		trans.exec();
		
		// retrieve the value
		System.out.println("Value is now: " + jedis.get("first"));
	}
}
