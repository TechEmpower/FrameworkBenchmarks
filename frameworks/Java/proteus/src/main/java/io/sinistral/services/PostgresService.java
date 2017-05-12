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
public class PostgresService   extends BaseService 
{

	private static Logger log = LoggerFactory.getLogger(PostgresService.class.getCanonicalName());

	@Inject
	@Named("postgres.hikaricp.jdbcUrl")
	protected String jdbcUrl;

	@Inject
	@Named("postgres.hikaricp.username")
	protected String username;
	
	@Inject
	@Named("postgres.hikaricp.password")
	protected String password;
	
	@Inject
	@Named("postgres.hikaricp.maximumPoolSize")
	protected Integer maximumPoolSize;
	
	@Inject
	@Named("postgres.hikaricp.minimumIdle")
	protected Integer minimumIdle;
	
	@Inject
	@Named("postgres.hikaricp.description")
	protected String description;

	protected HikariDataSource ds;
	
	
	
	/* (non-Javadoc)
	 * @see io.sinistral.proteus.services.BaseService#configure(com.google.inject.Binder)
	 */

	public void configure(Binder binder)
	{
		log.debug("Binding " + this.getClass().getSimpleName());
		binder.bind(PostgresService.class).toInstance(this);
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
