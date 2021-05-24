package net.officefloor.benchmark;

import java.net.Socket;

import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.ServerHttpConnection;
import net.officefloor.server.http.parse.HttpRequestParser;

/**
 * @author Daniel Sagenschneider
 */
public interface DatabaseOperations {

	/**
	 * Invoked on the {@link Socket} {@link Thread} to initiate {@link ThreadLocal}
	 * setup.
	 * 
	 * @param requestHandler {@link RequestHandler}.
	 */
	void threadSetup(RequestHandler<HttpRequestParser> requestHandler);

	/**
	 * Undertakes the db.
	 * 
	 * @param response   {@link HttpResponse}.
	 * @param connection {@link ServerHttpConnection}.
	 * @param context    {@link DatabaseOperationsContext}.
	 */
	void db(HttpResponse response, ServerHttpConnection connection, DatabaseOperationsContext context);

	/**
	 * Undertakes the queries.
	 * 
	 * @param queryCount Query count.
	 * @param response   {@link HttpResponse}.
	 * @param connection {@link ServerHttpConnection}.
	 * @param context    {@link DatabaseOperationsContext}.
	 */
	void queries(int queryCount, HttpResponse response, ServerHttpConnection connection,
			DatabaseOperationsContext context);

	/**
	 * Undertakes the fortunes.
	 * 
	 * @param response   {@link HttpResponse}.
	 * @param connection {@link ServerHttpConnection}.
	 * @param context    {@link DatabaseOperationsContext}.
	 */
	void fortunes(HttpResponse response, ServerHttpConnection connection, DatabaseOperationsContext context);

	/**
	 * Undertakes the update.
	 * 
	 * @param queryCount Query count.
	 * @param response   {@link HttpResponse}.
	 * @param connection {@link ServerHttpConnection}.
	 * @param context    {@link DatabaseOperationsContext}.
	 */
	void update(int queryCount, HttpResponse response, ServerHttpConnection connection,
			DatabaseOperationsContext context);

}
