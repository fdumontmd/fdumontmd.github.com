package jp.wakatta;

import java.util.List;

import redis.clients.jedis.Jedis;

public class RedisPop {
	public static void main(final String...args) throws Exception {
		boolean again = true;
		while (again) {
			System.out.println("Waiting for messages");
			Jedis jedis = new Jedis("localhost");
			List<String> msgs = jedis.blpop(300, "msg:queue");
			
			System.out.println("Messages received:");
			
			for (String msg: msgs) {
				System.out.println(msg);
				if (msg.equals("finish"))
					again = false;
			}
		}
		
		System.out.println("No more messages");
	}
}
