package net.officefloor.performance;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;

import javax.sql.DataSource;

import net.officefloor.plugin.socket.server.http.HttpHeader;
import net.officefloor.plugin.woof.WoofOfficeFloorSource;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Abstract functionality for test.
 * 
 * @author Daniel Sagenschneider
 */
public abstract class AbstractTestCase {

	/**
	 * Overridden by child tests for specific test behaviour.
	 * 
	 * @param client
	 *            {@link CloseableHttpClient}.
	 * @throws Exception
	 *             If fails.
	 */
	protected abstract void doRequestTest(CloseableHttpClient client)
			throws Exception;

	/*
	 * ================= Generic test functionality ==========================
	 */

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
	public void singleRequest() throws Exception {
		try (CloseableHttpClient client = HttpClientBuilder.create().build()) {
			this.doRequestTest(client);
		}
	}

	@Test
	public void performance() throws Throwable {

		// Avoid performance test if indicate to skip
		if (!("yes".equalsIgnoreCase(System.getProperty("doPerformanceTests",
				"no")))) {
			System.out.println("Skipping performance test for "
					+ this.getClass().getSimpleName());
			return;
		}

		final int threadCount = this.getParallelClientCount();
		final int iterationCount = this.getIterationCount();

		// Warm up the server
		System.out.println("===========================================");
		System.out.println(this.getClass().getSimpleName()
				+ "    (running with " + threadCount + " threads and "
				+ iterationCount + " iterations)");
		System.out.print("Warming up server ");
		System.out.flush();
		try (CloseableHttpClient client = HttpClientBuilder.create().build()) {
			for (int i = 0; i < (iterationCount * 2); i++) {
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

							// Determine progress mark
							int progressMark = iterationCount / 10;
							int progressValue = 0;

							// Under the request so many times
							for (int i = 0; i < iterationCount; i++) {

								// Undertake the request
								AbstractTestCase.this.doRequestTest(client);

								// Provide progress
								if ((i % progressMark) == 0) {
									System.out.print((progressValue++) + ", ");

									// Provide new line to avoid off to right
									if (threadIndex == 0) {
										System.out.println();
									}
								}
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

		// Report any failures
		Throwable firstFailure = null;
		synchronized (failures) {
			for (int t = 0; t < threads.length; t++) {
				Throwable failure = failures[t];
				if (failure != null) {

					// Log the failure
					System.err.println("=========== Failure for client " + t
							+ " ===========");
					failure.printStackTrace();

					// Capture the first failure
					if (firstFailure == null) {
						firstFailure = failure;
					}
				}
			}
		}
		if (firstFailure != null) {
			// Propagate the failure
			throw firstFailure;
		}

		// Determine statistics
		long duration = endTime - startTime;
		long totalRequestCount = threadCount * iterationCount;
		System.out.println();
		System.out.println(this.getClass().getSimpleName()
				+ " performance results:");
		System.out.println(totalRequestCount + " requests (" + threadCount
				+ " threads by " + iterationCount + " requests) completed in "
				+ duration + " milliseconds");
		System.out.println("Effectively 1 request every "
				+ (duration / (totalRequestCount * 1.0)) + " milliseconds");
	}

	/**
	 * Waits for the specified table to be available.
	 * 
	 * @param tableName
	 *            Name of the table.
	 * @param dataSource
	 *            {@link DataSource}.
	 * @throws SQLException
	 *             If fails to check if table available.
	 */
	protected void waitForTableToBeAvailable(String tableName,
			DataSource dataSource) throws SQLException {

		boolean isTableAvailable = false;
		long startTime = System.currentTimeMillis();
		do {

			// Attempt to determine if table available
			Connection connection = dataSource.getConnection();
			try {
				connection.createStatement().executeQuery(
						"SELECT * FROM " + tableName);
				isTableAvailable = true;
			} catch (SQLException ex) {
				isTableAvailable = false;
			} finally {
				connection.close();
			}

			if (!isTableAvailable) {

				// Determine if time out
				if ((System.currentTimeMillis() - 10000) > startTime) {
					Assert.fail("Time out in waiting for World table to be available");
				}

				// Allow some time for start up of database
				try {
					Thread.sleep(1000);
				} catch (InterruptedException ex) {
					// Carry on
				}
			}

		} while (!isTableAvailable);
	}

	/**
	 * Obtains the number of parallel clients.
	 * 
	 * @return Number of parallel clients.
	 */
	protected int getParallelClientCount() {
		return 100;
	}

	/**
	 * Obtains the iteration count for performance testing.
	 * 
	 * @return Iteration count for performance testing.
	 */
	protected int getIterationCount() {
		return 10000;
	}

	/**
	 * Asserts the {@link HttpResponse} is correct.
	 * 
	 * @param response
	 *            {@link HttpResponse}.
	 * @param statusCode
	 *            Expected status code.
	 * @param entity
	 *            Expected entity.
	 * @param headerNames
	 *            Expected {@link Header} instances.
	 * @throws IOException
	 *             If fails to obtain details from {@link HttpResponse}.
	 */
	protected void assertResponse(HttpResponse response, int statusCode,
			String entity, String... headerNames) throws IOException {

		// Obtain the entity
		String actualEntity = EntityUtils.toString(response.getEntity());

		// Ensure correct status code
		Assert.assertEquals("Should be successful: " + actualEntity, 200,
				response.getStatusLine().getStatusCode());

		// Validate the content
		Assert.assertEquals("Incorrect entity", entity, actualEntity);

		// Ensure have appropriate headers
		this.assertHeadersSet(response, headerNames);

	}

	/**
	 * Ensure all appropriate headers exist.
	 * 
	 * @param response
	 *            {@link HttpResponse}.
	 * @param headerNames
	 *            Expected header names.
	 */
	protected void assertHeadersSet(HttpResponse response,
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

	/**
	 * Asserts the {@link HttpHeader} is correct.
	 * 
	 * @param response
	 *            {@link HttpResponse}.
	 * @param headerName
	 *            {@link HttpHeader} name.
	 * @param headerValue
	 *            Expected {@link HttpHeader} value.
	 */
	protected void assertHeader(HttpResponse response, String headerName,
			String headerValue) {
		Header header = response.getFirstHeader(headerName);
		Assert.assertNotNull("Should have header " + headerName, header);
		Assert.assertEquals("Incorrect value for header " + headerName,
				headerValue, header.getValue());
	}

}