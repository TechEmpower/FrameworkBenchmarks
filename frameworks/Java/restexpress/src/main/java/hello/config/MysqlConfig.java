package hello.config;

import java.util.Properties;

import javax.sql.DataSource;

import org.apache.commons.dbcp.datasources.SharedPoolDataSource;
import org.restexpress.common.exception.ConfigurationException;

import com.mysql.cj.jdbc.MysqlConnectionPoolDataSource;

public class MysqlConfig {
	private static final String PREFIX = "mysql.";
	private static final String URI_PROPERTY = PREFIX + "uri";
	private static final String USE_CONFIGS_PROPERTY = PREFIX + "useConfigs";
	private static final String USER_NAME = PREFIX + "user";
	private static final String PASSWORD = PREFIX + "password";
	private static final String MAX_POOL_SIZE = PREFIX + "maxPoolSize";
	private static final String MIN_POOL_SIZE = PREFIX + "minPoolSize";

	private DataSource dataSource;

	public MysqlConfig(Properties p) {
		String uri = p.getProperty(URI_PROPERTY);
		String useConfigs = p.getProperty(USE_CONFIGS_PROPERTY);
		String dbUser = p.getProperty(USER_NAME);
		String dbPassword = p.getProperty(PASSWORD);
		int maxPoolSize = Integer.valueOf(p.getProperty(MAX_POOL_SIZE, "100"));
		int minPoolSize = Integer.valueOf(p.getProperty(MIN_POOL_SIZE, "3"));

		if (uri == null) {
			throw new ConfigurationException("Please define a MySQL URI for property: "
					+ URI_PROPERTY);
		}

		MysqlConnectionPoolDataSource ds = configureMysqlConnections(uri, dbUser, dbPassword,
				useConfigs);
		this.dataSource = configurePoolingDataSource(ds, maxPoolSize, minPoolSize);
	}

	private MysqlConnectionPoolDataSource configureMysqlConnections(String uri, String dbUser,
			String dbPassword, String useConfigs) {
		MysqlConnectionPoolDataSource ds = new MysqlConnectionPoolDataSource();
		ds.setUrl(uri);

		if (dbUser != null) {
			ds.setUser(dbUser);
		}

		if (dbPassword != null) {
			ds.setPassword(dbPassword);
		}

		if (useConfigs != null) {
			try {
				ds.setUseConfigs(useConfigs);
			} catch (java.sql.SQLException e) {
				throw new ConfigurationException("Unable to set DataSource config", e);
			}
		}
		return ds;
	}

	public DataSource getDataSource() {
		return dataSource;
	}

	private DataSource configurePoolingDataSource(MysqlConnectionPoolDataSource ds,
			int maxPoolSize, int minPoolSize) {
		SharedPoolDataSource spds = new SharedPoolDataSource();
		spds.setConnectionPoolDataSource(ds);
		spds.setMaxActive(maxPoolSize);
		spds.setMaxIdle(minPoolSize);
		return spds;
	}
}
