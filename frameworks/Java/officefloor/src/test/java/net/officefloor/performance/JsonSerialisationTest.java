package net.officefloor.performance;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;

/**
 * Ensure meet criteria for Test type 1: JSON serialization.
 * 
 * @author Daniel Sagenschneider
 */
public class JsonSerialisationTest extends AbstractTestCase {

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Request the JSON serialisation
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/json-service.woof"));

		// Validate the response
		this.assertResponse(response, 200, "{\"message\":\"Hello, World!\"}",
				"Content-Length", "Content-Type", "Server", "Date",
				"set-cookie");
		this.assertHeader(response, "Content-Type",
				"application/json; charset=UTF-8");
		this.assertHeader(response, "Content-Length", "27");
	}

}