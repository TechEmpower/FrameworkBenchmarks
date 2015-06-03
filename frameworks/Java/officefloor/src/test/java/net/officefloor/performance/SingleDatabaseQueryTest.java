package net.officefloor.performance;

import java.io.IOException;

import lombok.Data;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Ensures meets criteria for Test type 2: Single database query.
 * 
 * @author Daniel Sagenschneider.
 */
public class SingleDatabaseQueryTest extends AbstractDatabaseQueryTestCase {

	/**
	 * {@link ObjectMapper}.
	 */
	private final ObjectMapper mapper = new ObjectMapper();

	@Data
	public static class Result {
		private int id;
		private int randomNumber;
	}

	@Test
	public void ensureReadJson() throws IOException {
		String entity = "{\"id\":3217,\"randomNumber\":2149}";
		Result result = this.mapper.readValue(entity, Result.class);
		Assert.assertEquals("Incorrect id", 3217, result.getId());
		Assert.assertEquals("Incorrect random number", 2149,
				result.getRandomNumber());
	}

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Request the random database value
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/singleQuery-service.woof"));

		// Validate the response
		Assert.assertEquals("Should be successful", 200, response
				.getStatusLine().getStatusCode());
		this.assertHeadersSet(response, "Content-Length", "Content-Type",
				"Server", "Date", "set-cookie");
		this.assertHeader(response, "Content-Type",
				"application/json; charset=UTF-8");

		// Validate the correct random result
		String entity = EntityUtils.toString(response.getEntity());
		Result result = this.mapper.readValue(entity, Result.class);
		Assert.assertEquals("Incorrect random number",
				this.randomNumbers[result.id - 1], result.randomNumber);
	}

}