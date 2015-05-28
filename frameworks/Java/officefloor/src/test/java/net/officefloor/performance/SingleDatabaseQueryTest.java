package net.officefloor.performance;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import org.apache.http.impl.client.CloseableHttpClient;
import org.hsqldb.jdbc.jdbcDataSource;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

/**
 * Ensures meets criteria for Test type 2: Single database query.
 * 
 * @author Daniel Sagenschneider.
 */
public class SingleDatabaseQueryTest extends AbstractTestCase {

	/**
	 * URL for the database.
	 */
	private static final String DATABASE_URL = "jdbc:hsqldb:mem:exampleDb";

	/**
	 * User for the database.
	 */
	private static final String DATABASE_USER = "sa";

	/**
	 * Random numbers for validating results.
	 */
	private final int[] randomNumbers = new int[10000];

	@Before
	public void setupDatabase() throws SQLException {

		// Generate the random number values
		for (int i = 0; i < this.randomNumbers.length; i++) {
			this.randomNumbers[i] = (int) Math.round(Math.random() * 10000);
		}

		// Obtain connection via DataSource (sets up in memory database)
		jdbcDataSource dataSource = new jdbcDataSource();
		dataSource.setDatabase(DATABASE_URL);
		dataSource.setUser(DATABASE_USER);
		Connection connection = dataSource.getConnection();

		// Create the table
		Statement statement = connection.createStatement();
		statement
				.execute("CREATE TABLE World (id int primary key, randomNumber int)");

		// Load the values
		PreparedStatement insert = connection
				.prepareStatement("INSERT INTO World (id, randomNumber) VALUES (?, ?)");
		for (int i = 0; i < this.randomNumbers.length; i++) {
			insert.setInt(1, (i + 1));
			insert.setInt(2, this.randomNumbers[i]);
			insert.executeUpdate();
		}

		// Cleanup setup
		connection.close();
	}

	@After
	public void cleanupDatabase() throws SQLException {
		// Stop database for new instance each test
		DriverManager.getConnection(DATABASE_URL, DATABASE_USER, "")
				.createStatement().execute("SHUTDOWN IMMEDIATELY");
	}

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {
		Assert.fail("TODO implement request method");
	}

}