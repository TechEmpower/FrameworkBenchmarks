package net.officefloor.performance;

import java.nio.charset.Charset;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import net.officefloor.plugin.woof.WoofOfficeFloorSource;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.h2.jdbcx.JdbcConnectionPool;
import org.junit.After;
import org.junit.Before;

/**
 * Ensures meets criteria for Test type 4: Fortunes.
 * 
 * @author Daniel Sagenschneider.
 */
public class FortunesTest extends AbstractTestCase {

	/**
	 * Expected response.
	 */
	private static final String EXPECTED_RESPONSE = "<!DOCTYPE html>\n<html>\n"
			+ "<head><title>Fortunes</title></head>\n"
			+ "<body>\n<table>\n<tr><th>id</th><th>message</th></tr>\n"
			+ "<tr><td>11</td><td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;</td></tr>\n"
			+ "<tr><td>4</td><td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td></tr>\n"
			+ "<tr><td>5</td><td>A computer program does what you tell it to do, not what you want it to do.</td></tr>\n"
			+ "<tr><td>2</td><td>A computer scientist is someone who fixes things that aren't broken.</td></tr>\n"
			+ "<tr><td>8</td><td>A list is only as strong as its weakest link. &mdash; Donald Knuth</td></tr>\n"
			+ "<tr><td>0</td><td>Additional fortune added at request time.</td></tr>\n"
			+ "<tr><td>3</td><td>After enough decimal places, nobody gives a damn.</td></tr>\n"
			+ "<tr><td>7</td><td>Any program that runs right is obsolete.</td></tr>\n"
			+ "<tr><td>10</td><td>Computers make very fast, very accurate mistakes.</td></tr>\n"
			+ "<tr><td>6</td><td>Emacs is a nice operating system, but I prefer UNIX. &mdash; Tom Christaensen</td></tr>\n"
			+ "<tr><td>9</td><td>Feature: A bug with seniority.</td></tr>\n"
			+ "<tr><td>1</td><td>fortune: No such file or directory</td></tr>\n"
			+ "<tr><td>12</td><td>フレームワークのベンチマーク</td></tr>\n"
			+ "\n</table>\n</body>\n</html>";

	/**
	 * URL for the database.
	 */
	private static final String DATABASE_URL = "jdbc:h2:mem:exampleDb";

	/**
	 * User for the database.
	 */
	private static final String DATABASE_USER = "sa";

	@Before
	public void runApplication() throws Exception {
		// Start application after database setup
	}

	@Before
	public void setupDatabase() throws Exception {

		// Obtain connection via DataSource (sets up in memory database)
		JdbcConnectionPool dataSource = JdbcConnectionPool.create(DATABASE_URL,
				DATABASE_USER, "");
		Connection connection = dataSource.getConnection();

		// Create the table
		Statement statement = connection.createStatement();
		statement
				.execute("CREATE TABLE Fortune (id int primary key, message varchar(200))");

		// Load the values
		PreparedStatement insert = connection
				.prepareStatement("INSERT INTO Fortune (id, message) VALUES (?, ?)");
		this.loadEntry(insert, 1, "fortune: No such file or directory");
		this.loadEntry(insert, 2,
				"A computer scientist is someone who fixes things that aren't broken.");
		this.loadEntry(insert, 3,
				"After enough decimal places, nobody gives a damn.");
		this.loadEntry(insert, 4,
				"A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1");
		this.loadEntry(insert, 5,
				"A computer program does what you tell it to do, not what you want it to do.");
		this.loadEntry(insert, 6,
				"Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen");
		this.loadEntry(insert, 7, "Any program that runs right is obsolete.");
		this.loadEntry(insert, 8,
				"A list is only as strong as its weakest link. — Donald Knuth");
		this.loadEntry(insert, 9, "Feature: A bug with seniority.");
		this.loadEntry(insert, 10,
				"Computers make very fast, very accurate mistakes.");
		this.loadEntry(
				insert,
				11,
				"<script>alert(\"This should not be displayed in a browser alert box.\");</script>");
		this.loadEntry(insert, 12, "フレームワークのベンチマーク");

		// Ensure table is available
		this.waitForTableToBeAvailable("Fortune", dataSource);

		// Start the application
		WoofOfficeFloorSource.start();
	}

	@Override
	protected int getIterationCount() {
		return 1000;
	}

	/**
	 * Loads an row into the table.
	 * 
	 * @param insertStatement
	 *            {@link PreparedStatement}.
	 * @param identifier
	 *            Identifier.
	 * @param message
	 *            Message.
	 * @throws SQLException
	 *             If fails to insert the row.
	 */
	private void loadEntry(PreparedStatement insertStatement, int identifier,
			String message) throws SQLException {
		insertStatement.setInt(1, identifier);
		insertStatement.setString(2, message);
		insertStatement.executeUpdate();
	}

	@After
	public void cleanupDatabase() throws SQLException {
		// Stop database for new instance each test
		DriverManager.getConnection(DATABASE_URL, DATABASE_USER, "")
				.createStatement().execute("SHUTDOWN IMMEDIATELY");
	}

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Request the fortunes
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/fortune.woof"));

		// Validate the response
		this.assertResponse(response, 200, EXPECTED_RESPONSE, "Content-Length",
				"Content-Type", "Server", "Date", "set-cookie");
		this.assertHeader(response, "Content-Type", "text/html; charset=UTF-8");
		this.assertHeader(response, "Content-Length",
				String.valueOf(EXPECTED_RESPONSE.getBytes(Charset
						.forName("UTF-8")).length));
	}

}