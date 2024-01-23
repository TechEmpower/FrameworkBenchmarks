package net.officefloor.benchmark;

import java.net.Socket;

import net.officefloor.server.RequestHandler;
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
	 */
	void db(DbSendResponse sender);

	/**
	 * Undertakes the queries.
	 */
	void queries(int queriesCount, QueriesSendResponse sender);

	/**
	 * Undertakes the fortunes.
	 */
	void fortunes(FortunesSendResponse sender);

	/**
	 * Undertakes the update.
	 */
	void update(int updateCount, UpdateSendResponse sender);

	/**
	 * Undertakes the cached.
	 */
	void cached(int queryCount, CachedSendResponse sender);

}
