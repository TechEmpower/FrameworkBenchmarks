package net.officefloor.performance;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;

/**
 * Ensures meets criteria for Test type 5: Database updates.
 * 
 * @author Daniel Sagenschneider
 */
public class DatabaseUpdateTest extends AbstractDatabaseQueryTestCase {

	@Override
	protected int getParallelClientCount() {
		return 10;
	}

	@Override
	protected int getIterationCount() {
		return 100;
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

		// Obtain the entity
		String entity = EntityUtils.toString(response.getEntity());

		// Validate the response
		Assert.assertEquals("Should be successful: " + entity, 200, response
				.getStatusLine().getStatusCode());
		this.assertHeadersSet(response, "Content-Length", "Content-Type",
				"Server", "Date", "set-cookie");
		this.assertHeader(response, "Content-Type",
				"application/json; charset=UTF-8");

		// Validate the correct random result
		Result[] results = (Result[]) this.readResults(entity);
		Assert.assertEquals("Incorrect number of results", batchSize,
				results.length);

		// Ensure response value different to original value
		boolean isDifferent = false;
		for (Result result : results) {
			int responseValue = result.getRandomNumber();
			int originalValue = this.randomNumbers[result.getId() - 1];
			if (responseValue != originalValue) {
				isDifferent = true;
			}
		}
		Assert.assertTrue(
				"Should have at least one value different (if batch size big enough)",
				isDifferent || (batchSize <= 2));

		// Ensure values in database are different (ie have been updated)
		isDifferent = false;
		for (Result result : results) {
			int databaseValue = this.getDatabaseRandomNumber(result.getId());
			int originalValue = this.randomNumbers[result.getId() - 1];
			if (databaseValue != originalValue) {
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
		PreparedStatement selectStatement = this.connection
				.prepareStatement("SELECT id, randomNumber FROM World WHERE id = ?");
		selectStatement.setInt(1, identifier);
		ResultSet resultSet = selectStatement.executeQuery();
		try {
			Assert.assertTrue("No result for identifier " + identifier,
					resultSet.next());
			return resultSet.getInt("randomNumber");
		} finally {
			resultSet.close();
		}
	}

}