package net.officefloor.benchmark;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;

import net.officefloor.server.http.HttpClientRule;

/**
 * Tests the plain text.
 */
public class PlaintextTest {

	@Rule
	public HttpClientRule client = new HttpClientRule();

	@Test
	public void validRequest() throws Exception {

		// Run
		RawOfficeFloorMain.main(null);

		// Undertake test
		HttpResponse response = this.client.execute(new HttpGet("http://localhost:8080/plaintext"));
		assertEquals("Should be successful", 200, response.getStatusLine().getStatusCode());
		String entity = EntityUtils.toString(response.getEntity());
		assertEquals("Incorrect response", "Hello, World!", entity);
		assertEquals("Should be text", "text/plain", response.getFirstHeader("content-type").getValue());
		assertEquals("Incorrect server", "OF", response.getFirstHeader("server").getValue());
		assertNotNull("Should have date", response.getFirstHeader("date"));
	}

	@After
	public void stopListener() throws Exception {
		if (RawOfficeFloorMain.socketManager != null) {
			RawOfficeFloorMain.socketManager.shutdown();
		}
	}

}
