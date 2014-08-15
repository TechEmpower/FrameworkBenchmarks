package hello.config;

import hello.controller.JsonController;
import hello.controller.MongodbController;
import hello.controller.MysqlController;
import hello.controller.persistence.WorldsMongodbRepository;

import java.util.Properties;

import com.strategicgains.repoexpress.adapter.IdentiferAdapter;
import com.strategicgains.repoexpress.exception.InvalidObjectIdException;
import com.strategicgains.restexpress.Format;
import com.strategicgains.restexpress.util.Environment;

public class Configuration
extends Environment
{
	private static final String DEFAULT_EXECUTOR_THREAD_POOL_SIZE = "20";

	private static final String PORT_PROPERTY = "port";
	private static final String DEFAULT_FORMAT_PROPERTY = "default.format";
	private static final String BASE_URL_PROPERTY = "base.url";
	private static final String EXECUTOR_THREAD_POOL_SIZE = "executor.threadPool.size";

	private int port;
	private String defaultFormat;
	private String baseUrl;
	private int executorThreadPoolSize;

	private JsonController jsonController;
	private MysqlController mysqlController;
	private MongodbController mongodbController;

	@Override
	protected void fillValues(Properties p)
	{
		this.port = Integer.parseInt(p.getProperty(PORT_PROPERTY, "8080"));
		this.defaultFormat = p.getProperty(DEFAULT_FORMAT_PROPERTY, Format.JSON);
		this.baseUrl = p.getProperty(BASE_URL_PROPERTY, "http://localhost:" + String.valueOf(port));
		this.executorThreadPoolSize = Integer.parseInt(p.getProperty(EXECUTOR_THREAD_POOL_SIZE, DEFAULT_EXECUTOR_THREAD_POOL_SIZE));
		MongoConfig mongoSettings = new MongoConfig(p);
		MysqlConfig mysqlSettings = new MysqlConfig(p);
		initialize(mysqlSettings, mongoSettings);
	}

	private void initialize(MysqlConfig mysqlSettings, MongoConfig mongo)
	{
		jsonController = new JsonController();
		mysqlController = new MysqlController(mysqlSettings.getDataSource());
        WorldsMongodbRepository worldMongodbRepository = new WorldsMongodbRepository(mongo.getClient(), mongo.getDbName());
        worldMongodbRepository.setIdentifierAdapter(new IdentiferAdapter<Long>()
		{
			@Override
			public Long convert(String id) throws InvalidObjectIdException
			{
				return Long.valueOf(id);
			}
		});
		mongodbController = new MongodbController(worldMongodbRepository);
	}

	public String getDefaultFormat()
	{
		return defaultFormat;
	}

	public int getPort()
	{
		return port;
	}
	
	public String getBaseUrl()
	{
		return baseUrl;
	}
	
	public int getExecutorThreadPoolSize()
	{
		return executorThreadPoolSize;
	}

	public JsonController getJsonController()
	{
		return jsonController;
	}

	public MysqlController getMysqlController()
	{
		return mysqlController;
	}

	public MongodbController getMongodbController()
	{
		return mongodbController;
	}
}
