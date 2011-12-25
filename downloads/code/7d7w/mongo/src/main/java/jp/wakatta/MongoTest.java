package jp.wakatta;

import static java.lang.Math.floor;
import static java.lang.Math.random;
import static java.lang.Math.round;

import java.util.List;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import com.mongodb.Mongo;

public class MongoTest {
	public static void main(String...args) {
		try {
			// connect to the database server
			// note: use 127.0.0.1 instead of localhost
			// as mongoDB only listen to the loopback
			// interface and not the ethernet one
			Mongo m = new Mongo("127.0.0.1");
			
			// make sure we're in a clean state
			m.dropDatabase("java");
			
			// create and access the database
			DB db = m.getDB("java");
			
			// create collection and populate it
			DBCollection phones = db.getCollection("phones");
			populatePhones( 800, 5550000, 5650000 , phones);
			
			// create index
			phones.createIndex(new BasicDBObject("display", 1));
			
			// list the indexes
			List<DBObject> list = phones.getIndexInfo();

	        for (DBObject o : list) {
	            System.out.println(o);
	        }
	        
	        // close and cleanup
	        m.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	public static void populatePhones(long area, long start, long stop, DBCollection coll) {
		for (long i=start; i < stop; i++) {
			long country = round(floor(1 + (random() * 8)));
			long num = (country * 10000000000l) + (area * 10000000) + i;
			BasicDBObject phone = new BasicDBObject();
			BasicDBObject components = new BasicDBObject();
			phone.put("_id", num);
			components.put("country", country);
			components.put("area", area);
			components.put("prefix", (i * 10000));
			components.put("number", i);
			phone.put("components", components);
			phone.put("display", "+" + country + " " + area + "-" + i);
			coll.insert(phone);
		}
	}
}
