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
	@Named("mysql.hikaricp.maximumPoolSize")
	protected Integer maximumPoolSize;
	 
	
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
 		
 		this.ds = new HikariDataSource();

		this.ds.setJdbcUrl(jdbcUrl);
		this.ds.setUsername(username);
		this.ds.setPassword(password);  
		this.ds.setMaximumPoolSize(maximumPoolSize);
 
		
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
