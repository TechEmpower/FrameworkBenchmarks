package hello.config;

import java.net.UnknownHostException;
import java.util.Properties;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import com.strategicgains.restexpress.exception.ConfigurationException;

public class MongoConfig
{
	private static final String URI_PROPERTY = "mongodb.uri";

	private String dbName;
	private MongoClient client;

    public MongoConfig(Properties p)
    {
		String uri = p.getProperty(URI_PROPERTY);

		if (uri == null)
		{
			throw new ConfigurationException("Please define a MongoDB URI for property: " + URI_PROPERTY);
		}

		MongoClientURI mongoUri = new MongoClientURI(uri);
		dbName = mongoUri.getDatabase();
		try
        {
	        client = new MongoClient(mongoUri);
        }
        catch (UnknownHostException e)
        {
        	throw new ConfigurationException(e);
        }
    }

	public String getDbName()
	{
		return dbName;
	}

	public MongoClient getClient()
	{
		return client;
	}
}
