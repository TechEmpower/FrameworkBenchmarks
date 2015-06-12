package net.officefloor.performance;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;
import org.junit.Before;

/**
 * Ensures meets criteria for Test type 5: Database updates.
 * 
 * @author Daniel Sagenschneider
 */
public class DatabaseUpdateTest extends AbstractDatabaseQueryTestCase {

	/**
	 * {@link PreparedStatement} to select the row.
	 */
	private PreparedStatement selectStatement;

	@Before
	public void loadSelectStatement() throws SQLException {
		this.selectStatement = this.connection
				.prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?");
	}

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Obtain the next index
		int index = this.nextIndex();

		// Obtain the query string
		String queryString = this.getQueryString(index);

		// Obtain the expected batch size
		int batchSize = this.getBatchSize(index);

		// Request the results
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/databaseUpdate-service.woof"
						+ queryString));

		// Validate the response
		Assert.assertEquals("Should be successful", 200, response
				.getStatusLine().getStatusCode());
		this.assertHeadersSet(response, "Content-Length", "Content-Type",
				"Server", "Date", "set-cookie");
		this.assertHeader(response, "Content-Type",
				"application/json; charset=UTF-8");

		// Validate the correct random result
		String entity = EntityUtils.toString(response.getEntity());
		Result[] results = (Result[]) this.readResults(entity);
		Assert.assertEquals("Incorrect number of results", batchSize,
				results.length);

		// Ensure at least one value is within database (other requests may
		// change some rows)
		boolean isDifferent = false;
		for (Result result : results) {
			int randomNumber = this.getDatabaseRandomNumber(result.getId());
			if (randomNumber != this.randomNumbers[result.getId() - 1]) {
				isDifferent = true;
			}
		}
		Assert.assertTrue(
				"Should have at least one number different (if batch size big enough)",
				isDifferent || (batchSize <= 5));

		// Ensure values in database are different (ie have been updated)
		isDifferent = false;
		for (int i = 0; i < this.randomNumbers.length; i++) {
			int randomNumber = this.getDatabaseRandomNumber(i + 1);
			if (randomNumber != this.randomNumbers[i]) {
				isDifferent = true;
			}
		}
		Assert.assertTrue(
				"Database should have changed (except unlikely case of same random number)",
				isDifferent || (batchSize <= 2));
	}

	/**
	 * Obtains the random number in the database for the identifier.
	 * 
	 * @param identifier
	 *            Identifier.
	 * @return Random number.
	 * @throws SQLException
	 *             If fails to obtain the random number.
	 */
	private int getDatabaseRandomNumber(int identifier) throws SQLException {
		this.selectStatement.setInt(1, identifier);
		ResultSet resultSet = this.selectStatement.executeQuery();
		try {
			Assert.assertTrue(resultSet.next());
			return resultSet.getInt("randomNumber");
		} finally {
			resultSet.close();
		}
	}

}