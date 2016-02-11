package net.officefloor.performance;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;

/**
 * Ensures meets criteria for Test type 6: Plaintext.
 * 
 * @author Daniel Sagenschneider.
 */
public class PlainTextTest extends AbstractTestCase {

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Request the plain text
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/plaintext.woof"));

		// Validate the response
		this.assertResponse(response, 200, "Hello, World!", "Content-Length",
				"Content-Type", "Server", "Date", "set-cookie");
		this.assertHeader(response, "Content-Type", "text/plain; charset=UTF-8");
		this.assertHeader(response, "Content-Length", "13");
	}

}