package jp.wakatta;

import redis.clients.jedis.Jedis;

public class RedisPush {
	public static void main(final String...args) throws Exception {
		Jedis jedis = new Jedis("localhost");
		jedis.lpush("msg:queue", "A new message");
		System.out.println("Message inserted");
	}
}
