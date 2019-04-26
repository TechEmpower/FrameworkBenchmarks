package hello.config;

import java.util.Properties;

import org.restexpress.common.exception.ConfigurationException;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;

public class MongoConfig {
	private static final String URI_PROPERTY = "mongodb.uri";

	private String dbName;
	private MongoClient client;

	public MongoConfig(Properties p) {
		String uri = p.getProperty(URI_PROPERTY);

		if (uri == null) {
			throw new ConfigurationException("Please define a MongoDB URI for property: "
					+ URI_PROPERTY);
		}

		MongoClientURI mongoUri = new MongoClientURI(uri);
		dbName = mongoUri.getDatabase();
		client = new MongoClient(mongoUri);
	}

	public String getDbName() {
		return dbName;
	}

	public MongoClient getClient() {
		return client;
	}
}
