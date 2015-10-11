package net.officefloor.performance;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import lombok.Data;
import net.officefloor.plugin.woof.WoofOfficeFloorSource;

import org.h2.jdbcx.JdbcConnectionPool;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.ArrayType;

/**
 * Abstract functionality for a database query test.
 * 
 * @author Daniel Sagenschneider
 */
public abstract class AbstractDatabaseQueryTestCase extends AbstractTestCase {

	/**
	 * URL for the database.
	 */
	private static final String DATABASE_URL = "jdbc:h2:mem:exampleDb";

	/**
	 * User for the database.
	 */
	private static final String DATABASE_USER = "sa";

	/**
	 * Random numbers for validating results.
	 */
	protected final int[] randomNumbers = new int[10000];

	/**
	 * {@link ObjectMapper}.
	 */
	private final ObjectMapper mapper = new ObjectMapper();

	/**
	 * {@link ArrayType} for parsing the JSON.
	 */
	private final ArrayType arrayType = this.mapper.getTypeFactory()
			.constructArrayType(Result.class);

	/**
	 * Number of items to request.
	 */
	private volatile int requestBatchIndex = 0;

	/**
	 * Listing of request batch sizes.
	 */
	private final String[] queryValues = new String[] { null, "INVALID", "0",
			"1", "5", "10", "15", "20", "500", "501" };

	/**
	 * Corresponding list of the resulting request batch size.
	 */
	private final int[] requestBatchSizes = new int[] { 1, 1, 1, 1, 5, 10, 15,
			20, 500, 500 };

	/**
	 * {@link Connection}.
	 */
	protected Connection connection;

	/**
	 * Result from JSON response.
	 */
	@Data
	public static class Result {
		private int id;
		private int randomNumber;
	}

	@Before
	public void runApplication() throws Exception {
		// Start application after database setup
	}

	@Before
	public void setupDatabase() throws Exception {

		// Generate the random number values
		for (int i = 0; i < this.randomNumbers.length; i++) {
			this.randomNumbers[i] = (int) Math.round(Math.random() * 10000);
		}

		// Obtain connection via DataSource (sets up in memory database)
		JdbcConnectionPool dataSource = JdbcConnectionPool.create(DATABASE_URL,
				DATABASE_USER, "");
		this.connection = dataSource.getConnection();

		// Create the table
		Statement statement = this.connection.createStatement();
		statement
				.execute("CREATE TABLE World (id int primary key, randomNumber int)");

		// Load the values
		PreparedStatement insert = this.connection
				.prepareStatement("INSERT INTO World (id, randomNumber) VALUES (?, ?)");
		for (int i = 0; i < this.randomNumbers.length; i++) {
			insert.setInt(1, (i + 1));
			insert.setInt(2, this.randomNumbers[i]);
			insert.executeUpdate();
		}

		// Wait until table available on another connection
		this.waitForTableToBeAvailable("World", dataSource);

		// Start the application
		WoofOfficeFloorSource.start();
	}

	@After
	public void cleanupDatabase() throws SQLException {

		// Allow some time for clean up of server
		try {
			Thread.sleep(1000);
		} catch (InterruptedException ex) {
			// Carry on
		}

		// Stop database for new instance each test
		this.connection.createStatement().execute("SHUTDOWN IMMEDIATELY");

		// Allow some time for shutdown of database
		try {
			Thread.sleep(1000);
		} catch (InterruptedException ex) {
			// Carry on
		}
	}

	@Override
	protected int getIterationCount() {
		return 1000;
	}

	/**
	 * Obtains the next index.
	 * 
	 * @return Next index.
	 */
	protected int nextIndex() {
		// Obtain the next index
		int index = this.requestBatchIndex;

		// Provide the index for next request (duplicate batch sizes ok)
		this.requestBatchIndex = (index + 1) % this.requestBatchSizes.length;

		// Return the index
		return index;
	}

	/**
	 * Obtains the query string.
	 * 
	 * @param index
	 *            Batch index.
	 * @return Query string.
	 */
	protected String getQueryString(int index) {
		String queryString = this.queryValues[index];
		queryString = (queryString == null ? "" : "?queries=" + queryString);
		return queryString;
	}

	/**
	 * Obtains the expected batch size.
	 * 
	 * @param index
	 *            Batch index.
	 * @return Expected batch size.
	 */
	protected int getBatchSize(int index) {
		return this.requestBatchSizes[index];
	}

	/**
	 * Reads the {@link Result}.
	 * 
	 * @param entity
	 *            Entity content.
	 * @return {@link Result}.
	 * @throws IOException
	 *             If fails to read {@link Result}.
	 */
	protected Result readResult(String entity) throws IOException {
		return this.mapper.readValue(entity, Result.class);
	}

	@Test
	public void ensureReadResult() throws IOException {
		String entity = "{\"id\":3217,\"randomNumber\":2149}";
		Result result = this.mapper.readValue(entity, Result.class);
		assertResult(result, 3217, 2149);
	}

	/**
	 * Reads the {@link Result} instances from the entity.
	 * 
	 * @param entity
	 *            Entity content.
	 * @return {@link Result} instances.
	 * @throws IOException
	 *             If fails to read {@link Result} instances.
	 */
	protected Result[] readResults(String entity) throws IOException {
		return (Result[]) this.mapper.readValue(entity, this.arrayType);
	}

	@Test
	public void ensureReadResults() throws IOException {
		String entity = "[{\"id\":4174,\"randomNumber\":331}"
				+ ",{\"id\":51,\"randomNumber\":6544}"
				+ ",{\"id\":4462,\"randomNumber\":952}"
				+ ",{\"id\":2221,\"randomNumber\":532}"
				+ ",{\"id\":9276,\"randomNumber\":3097}"
				+ ",{\"id\":3056,\"randomNumber\":7293}"
				+ ",{\"id\":6964,\"randomNumber\":620}"
				+ ",{\"id\":675,\"randomNumber\":6601}"
				+ ",{\"id\":8414,\"randomNumber\":6569}"
				+ ",{\"id\":2753,\"randomNumber\":4065}]";
		Result[] results = (Result[]) this.mapper.readValue(entity,
				this.arrayType);
		Assert.assertEquals("Incorrect number of elements", 10, results.length);
		assertResult(results[0], 4174, 331);
		assertResult(results[1], 51, 6544);
		assertResult(results[2], 4462, 952);
		assertResult(results[3], 2221, 532);
		assertResult(results[4], 9276, 3097);
		assertResult(results[5], 3056, 7293);
		assertResult(results[6], 6964, 620);
		assertResult(results[7], 675, 6601);
		assertResult(results[8], 8414, 6569);
		assertResult(results[9], 2753, 4065);
	}

	private static void assertResult(Result result, int id, int randomNumber) {
		Assert.assertEquals("Incorrect identifier", id, result.getId());
		Assert.assertEquals("Incorrect random number", randomNumber,
				result.getRandomNumber());
	}

}