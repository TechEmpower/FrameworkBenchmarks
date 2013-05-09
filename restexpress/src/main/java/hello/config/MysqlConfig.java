package hello.config;

import java.util.Properties;

import javax.sql.DataSource;

import com.mysql.jdbc.jdbc2.optional.MysqlConnectionPoolDataSource;
import com.strategicgains.restexpress.exception.ConfigurationException;

public class MysqlConfig
{
	private static final String URI_PROPERTY = "mysql.uri";
	private static final String USE_CONFIGS_PROPERTY = "mysql.useConfigs";

	private DataSource dataSource;

	public MysqlConfig(Properties p)
    {
		String uri = p.getProperty(URI_PROPERTY);
		String useConfigs = p.getProperty(USE_CONFIGS_PROPERTY);

		if (uri == null)
		{
			throw new ConfigurationException("Please define a MySQL URI for property: " + URI_PROPERTY);
		}
		
		MysqlConnectionPoolDataSource ds = new MysqlConnectionPoolDataSource();
		ds.setUrl(uri);
		
		if (useConfigs != null)
		{
			ds.setUseConfigs(useConfigs);
		}

		this.dataSource = ds;
    }

	public DataSource getDataSource()
    {
	    return dataSource;
    }
}
