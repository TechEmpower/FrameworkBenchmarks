package net.officefloor.performance;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.junit.Assert;

/**
 * Runs application and ensures a page is available.
 */
public class PlainTextTest extends AbstractTestCase {

	/**
	 * Undertakes a single test.
	 * 
	 * @param client
	 *            {@link CloseableHttpClient}.
	 * @throws Exception
	 *             If request fails.
	 */
	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Request the plain text
		HttpResponse response = client.execute(new HttpGet(
				"http://localhost:7878/plaintext.woof"));
		Assert.assertEquals(200, response.getStatusLine().getStatusCode());

		// Validate the content
		String entity = EntityUtils.toString(response.getEntity());
		Assert.assertEquals("Hello, World!", entity);

		// Ensure have appropriate headers
		assertHeaders(response, "Content-Length", "Content-Type", "Server",
				"Date", "set-cookie");

		// Ensure correct content type
		Header contentTypeHeader = response.getFirstHeader("Content-Type");
		Assert.assertEquals("Incorrect content type",
				"text/plain; charset=UTF-8", contentTypeHeader.getValue());
	}

	/**
	 * Ensure all appropriate headers exist.
	 * 
	 * @param response
	 *            {@link HttpResponse}.
	 * @param headerNames
	 *            Expected header names.
	 */
	private static void assertHeaders(HttpResponse response,
			String... headerNames) {

		// Obtain the headers
		Header[] headers = response.getAllHeaders();

		// Create the listing of expected headers
		StringBuilder expectedHeadersText = new StringBuilder();
		boolean isFirst = true;
		for (String headerName : headerNames) {
			if (!isFirst) {
				expectedHeadersText.append(", ");
			}
			expectedHeadersText.append(headerName);
		}

		// Create the header text
		StringBuilder actualHeaderText = new StringBuilder();
		for (Header header : headers) {
			actualHeaderText.append(header.getName() + ": " + header.getValue()
					+ "\n");
		}

		// Validate correct number of headers
		Assert.assertEquals("Incorrect number of headers ("
				+ expectedHeadersText.toString() + ")\n\n" + actualHeaderText,
				headerNames.length, headers.length);
	}

}