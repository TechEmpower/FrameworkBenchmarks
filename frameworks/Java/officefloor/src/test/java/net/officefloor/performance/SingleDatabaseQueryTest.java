package net.officefloor.performance;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;

/**
 * Ensures meets criteria for Test type 2: Single database query.
 * 
 * @author Daniel Sagenschneider.
 */
public class SingleDatabaseQueryTest extends AbstractDatabaseQueryTestCase {

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Request the random database value
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/singleQuery-service.woof"));

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
		Result result = this.readResult(entity);
		Assert.assertEquals("Incorrect random number",
				this.randomNumbers[result.getId() - 1],
				result.getRandomNumber());
	}

}