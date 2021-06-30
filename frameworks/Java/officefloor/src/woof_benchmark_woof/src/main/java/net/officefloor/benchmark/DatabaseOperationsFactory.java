package net.officefloor.benchmark;

import java.net.Socket;

/**
 * Factory for the {@link DatabaseOperations}.
 * 
 * @author Daniel Sagenschneider
 */
public interface DatabaseOperationsFactory {

	/**
	 * Creates the {@link DatabaseOperations}.
	 * 
	 * @param socketCount Number of server {@link Socket} instances.
	 * @param server      Name of database server.
	 * @param port        Port of database.
	 * @param database    Name of database within server.
	 * @param username    Username.
	 * @param password    Password.
	 * @return {@link DatabaseOperations}.
	 * @throws Throwable If fails to create {@link DatabaseOperations}.
	 */
	DatabaseOperations createDatabaseOperations(int socketCount, String server, int port, String database,
			String username, String password) throws Throwable;

}