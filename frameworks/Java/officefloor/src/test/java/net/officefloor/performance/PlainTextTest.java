package net.officefloor.performance;

import net.officefloor.plugin.woof.WoofOfficeFloorSource;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Runs application and ensures a page is available.
 */
public class PlainTextTest {

	@Before
	public void runApplication() throws Exception {
		// Start the application
		WoofOfficeFloorSource.start();
	}

	@After
	public void stopApplication() throws Exception {
		// Stop the application
		WoofOfficeFloorSource.stop();
	}

	@Test
	public void ensurePlainText() throws Exception {
		try (CloseableHttpClient client = HttpClientBuilder.create().build()) {
			this.doRequestTest(client);
		}
	}

	@Test
	public void performancePlainText() throws Exception {

		final int threadCount = 100;
		final int iterationCount = 10000;

		// Warm up the server
		System.out.print("Warming up server ");
		System.out.flush();
		try (CloseableHttpClient client = HttpClientBuilder.create().build()) {
			for (int i = 0; i < 20000; i++) {
				this.doRequestTest(client);
				if (i % 1000 == 0) {
					System.out.print(".");
					System.out.flush();
				}
			}
		}
		System.out.println(" warmed up");

		// Create the threads
		Thread[] threads = new Thread[threadCount];
		final boolean[] threadComplete = new boolean[threads.length];
		final Throwable[] failures = new Throwable[threads.length];
		for (int t = 0; t < threads.length; t++) {
			final int threadIndex = t;
			threadComplete[threadIndex] = false; // Not yet complete
			failures[threadIndex] = null; // no initial failure
			threads[threadIndex] = new Thread() {
				@Override
				public void run() {
					try {
						try (CloseableHttpClient client = HttpClientBuilder
								.create().build()) {

							// Under the request so many times
							for (int i = 0; i < iterationCount; i++) {
								PlainTextTest.this
										.doRequestTest(client);
							}
						}
					} catch (Throwable ex) {
						// Record failure
						synchronized (failures) {
							failures[threadIndex] = ex;
						}
					} finally {
						// Notify complete
						synchronized (threadComplete) {
							threadComplete[threadIndex] = true;
							threadComplete.notify();
						}
					}
				}
			};
		}

		// Capture start time
		long startTime = System.currentTimeMillis();

		// Kick off the threads
		System.out.print("Running requests ...");
		System.out.flush();
		for (int t = 0; t < threads.length; t++) {
			threads[t].start();
		}

		// Wait for completion
		synchronized (threadComplete) {

			// Determine if complete
			boolean isComplete = false;
			do {
				// Determine if all threads a complete
				isComplete = true;
				for (int t = 0; t < threads.length; t++) {
					if (!threadComplete[t]) {
						isComplete = false;
					}
				}

				// Wait some time if not complete
				if (!isComplete) {
					threadComplete.wait(1000);
				}

			} while (!isComplete);
		}

		// Capture the end time
		long endTime = System.currentTimeMillis();
		System.out.println(" requests completed");

		// Determine statistics
		long duration = endTime - startTime;
		long totalRequestCount = threadCount * iterationCount;
		System.out.println();
		System.out.println("Performance results:");
		System.out.println(totalRequestCount + " requests (" + threadCount
				+ " threads by " + iterationCount + " requests) completed in "
				+ duration + " milliseconds");
		System.out.println("Effectively 1 request every "
				+ (duration / (totalRequestCount * 1.0)) + " milliseconds");
	}

	/**
	 * Undertakes a single test.
	 * 
	 * @param client
	 *            {@link CloseableHttpClient}.
	 * @throws Exception
	 *             If request fails.
	 */
	private void doRequestTest(CloseableHttpClient client) throws Exception {

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