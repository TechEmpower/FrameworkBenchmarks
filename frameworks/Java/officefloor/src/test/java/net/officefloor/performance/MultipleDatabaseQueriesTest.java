package net.officefloor.performance;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;

/**
 * Ensures meets criteria for Test type 3: Multiple database queries.
 * 
 * @author Daniel Sagenschneider.
 */
public class MultipleDatabaseQueriesTest extends AbstractDatabaseQueryTestCase {

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
				"http://localhost:7878/multipleQueries-service.woof"
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
		for (int i = 0; i < batchSize; i++) {
			Result result = results[i];
			Assert.assertEquals("Incorrect random number for item " + i,
					this.randomNumbers[result.getId() - 1],
					result.getRandomNumber());
		}
	}

}