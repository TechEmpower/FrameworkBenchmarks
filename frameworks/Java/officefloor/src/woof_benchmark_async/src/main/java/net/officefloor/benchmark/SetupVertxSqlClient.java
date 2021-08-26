package net.officefloor.benchmark;

import io.vertx.sqlclient.PoolOptions;
import net.officefloor.frame.api.source.ServiceContext;
import net.officefloor.vertx.sqlclient.VertxSqlPoolConfigurer;
import net.officefloor.vertx.sqlclient.VertxSqlPoolConfigurerContext;
import net.officefloor.vertx.sqlclient.VertxSqlPoolConfigurerServiceFactory;

/**
 * Sets up the {@link PoolOptions}.
 * 
 * @author Daniel Sagenschneider
 */
public class SetupVertxSqlClient implements VertxSqlPoolConfigurer, VertxSqlPoolConfigurerServiceFactory {

	/*
	 * ================ VertxSqlPoolConfigurerServiceFactory =================
	 */

	@Override
	public VertxSqlPoolConfigurer createService(ServiceContext context) throws Throwable {
		return this;
	}

	/*
	 * ======================= VertxSqlPoolConfigurer =========================
	 */

	@Override
	public void configure(VertxSqlPoolConfigurerContext context) throws Exception {

		// Ensure adequate number of connections
		final int MAX_POOL_SIZE = 512;
		System.out.println("Setting max pool size to " + MAX_POOL_SIZE);
		context.getPoolOptions().setMaxSize(MAX_POOL_SIZE);

		// Configure options
		context.getSqlConnectOptions().setCachePreparedStatements(true);
	}

}