package hello.config;

import java.util.Properties;

import org.restexpress.Format;
import org.restexpress.util.Environment;

import com.strategicgains.repoexpress.mongodb.MongoConfig;

import hello.controller.JsonController;
import hello.controller.MongodbController;
import hello.controller.MysqlController;
import hello.controller.PlaintextController;
import hello.controller.QueriesMongodbController;
import hello.controller.QueriesMysqlController;
import hello.controller.persistence.WorldsMongodbRepository;

public class Configuration extends Environment {
	private static final String DEFAULT_EXECUTOR_THREAD_POOL_SIZE = "20";

	private static final String PORT_PROPERTY = "port";
	private static final String DEFAULT_FORMAT_PROPERTY = "default.format";
	private static final String BASE_URL_PROPERTY = "base.url";
	private static final String EXECUTOR_THREAD_POOL_SIZE = "executor.threadPool.size";

	private int port;
	private String defaultFormat;
	private String baseUrl;
	private int executorThreadPoolSize;
	private Database database = null;

	private PlaintextController plaintextController;
	private JsonController jsonController;
	private MysqlController mysqlController;
	private QueriesMysqlController queriesMysqlController;
	private MongodbController mongodbController;
	private QueriesMongodbController queriesMongodbController;

	@Override
	protected void fillValues(Properties p) {
		this.port = Integer.parseInt(p.getProperty(PORT_PROPERTY, "8080"));
		this.defaultFormat = p.getProperty(DEFAULT_FORMAT_PROPERTY, Format.JSON);
		this.baseUrl = p.getProperty(BASE_URL_PROPERTY, "http://localhost:" + String.valueOf(port));
		this.executorThreadPoolSize = Integer.parseInt(p.getProperty(EXECUTOR_THREAD_POOL_SIZE,
				DEFAULT_EXECUTOR_THREAD_POOL_SIZE));
		if (p.containsKey("mongodb.uri")) {
			MongoConfig mongoSettings = new MongoConfig(p);
			initialize(mongoSettings);
			database = Database.MongoDB;
		} else if (p.containsKey("mysql.uri")) {
			MysqlConfig mysqlSettings = new MysqlConfig(p);
			initialize(mysqlSettings);
			database = Database.MySQL;
		}
	}

	private void initialize(MongoConfig mongo) {
		WorldsMongodbRepository worldMongodbRepository = new WorldsMongodbRepository(
				mongo.getClient(), mongo.getDbName());
		plaintextController = new PlaintextController();
		jsonController = new JsonController();
		mongodbController = new MongodbController(worldMongodbRepository);
		queriesMongodbController = new QueriesMongodbController(worldMongodbRepository);
	}
	
	private void initialize(MysqlConfig mysqlSettings) {
		mysqlController = new MysqlController(mysqlSettings.getDataSource());
		queriesMysqlController = new QueriesMysqlController(mysqlSettings.getDataSource());
	}

	public String getDefaultFormat() {
		return defaultFormat;
	}

	public int getPort() {
		return port;
	}

	public String getBaseUrl() {
		return baseUrl;
	}

	public int getExecutorThreadPoolSize() {
		return executorThreadPoolSize;
	}

	public Database getDatabase() {
		return database;
	}

	public PlaintextController getPlaintextController() {
		return plaintextController;
	}
	
	public JsonController getJsonController() {
		return jsonController;
	}

	public MysqlController getMysqlController() {
		return mysqlController;
	}

	public QueriesMysqlController getQueriesMysqlController() {
		return queriesMysqlController;
	}

	public MongodbController getMongodbController() {
		return mongodbController;
	}

	public QueriesMongodbController getQueriesMongodbController() {
		return queriesMongodbController;
	}
}