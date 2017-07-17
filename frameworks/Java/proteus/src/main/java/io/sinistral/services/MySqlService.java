/**
 * 
 */
package io.sinistral.services;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.Binder;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import io.sinistral.proteus.services.BaseService;

/**
 * @author jbauer
 *
 */
@Singleton
public class MySqlService   extends BaseService 
{

	private static Logger log = LoggerFactory.getLogger(MySqlService.class.getCanonicalName());

	@Inject
	@Named("mysql.hikaricp.jdbcUrl")
	protected String jdbcUrl;

	@Inject
	@Named("mysql.hikaricp.username")
	protected String username;
	
	@Inject
	@Named("mysql.hikaricp.password")
	protected String password;
	
	@Inject
	@Named("mysql.hikaricp.dataSourceClassName")
	protected String dataSourceClassName;
	
	@Inject
	@Named("mysql.hikaricp.ds.databaseName")
	protected String databaseName;
	
	@Inject
	@Named("mysql.hikaricp.ds.serverName")
	protected String serverName;
	
	@Inject
	@Named("mysql.hikaricp.ds.portNumber")
	protected Integer portNumber;
	
	@Inject
	@Named("mysql.hikaricp.maximumPoolSize")
	protected Integer maximumPoolSize;
	
	@Inject
	@Named("mysql.hikaricp.minimumIdle")
	protected Integer minimumIdle;
	
	@Inject
	@Named("mysql.hikaricp.ds.useServerPrepStmts")
	protected Boolean useServerPrepStmts;
	
	@Inject
	@Named("mysql.hikaricp.ds.cachePrepStmts")
	protected Boolean cachePrepStmts;
	
	@Inject
	@Named("mysql.hikaricp.ds.cacheCallableStmts")
	protected Boolean useCacheCallableStmts;
	
	@Inject
	@Named("mysql.hikaricp.ds.prepStmtCacheSize")
	protected Integer prepStmtCacheSize;
	
	@Inject
	@Named("mysql.hikaricp.ds.prepStmtCacheSqlLimit")
	protected Integer prepStmtCacheSqlLimit;
	
	
	protected HikariDataSource ds;
	
	
	
	/* (non-Javadoc)
	 * @see io.sinistral.proteus.services.BaseService#configure(com.google.inject.Binder)
	 */

	public void configure(Binder binder)
	{
		log.debug("Binding " + this.getClass().getSimpleName());
		binder.bind(MySqlService.class).toInstance(this);
	}

	@Override
	protected void startUp() throws Exception
	{
		super.startUp();
 		
		HikariConfig config = new HikariConfig();
		 
		config.setJdbcUrl(jdbcUrl);
		config.setUsername(username);
		config.setPassword(password); 
		config.setMinimumIdle(minimumIdle);
		config.setMaximumPoolSize(maximumPoolSize);
		config.addDataSourceProperty("databaseName", databaseName);
		config.addDataSourceProperty("useSSL",false);
		config.addDataSourceProperty("UseServerPrepStmts", useServerPrepStmts);
		config.addDataSourceProperty("CachePrepStmts", cachePrepStmts);
		config.addDataSourceProperty("CacheCallableStmts", useCacheCallableStmts);
		config.addDataSourceProperty("PrepStmtCacheSize", prepStmtCacheSize);
		config.addDataSourceProperty("PrepStmtCacheSqlLimit", prepStmtCacheSqlLimit);

		this.ds = new HikariDataSource(config);
		
		log.info( this.getClass().getSimpleName() + " started with ds: " + ds);  
	}
	
	public Connection getConnection() throws SQLException
	{
		return this.ds.getConnection();
	}

	
	@Override
	protected void shutDown() throws Exception
	{
		super.shutDown();

		this.ds.close();
 	}

}
