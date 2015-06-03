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
import com.fasterxml.jackson.databind.type.ArrayType;

/**
 * Ensures meets criteria for Test type 3: Multiple database queries.
 * 
 * @author Daniel Sagenschneider.
 */
public class MultipleDatabaseQueriesTest extends AbstractDatabaseQueryTestCase {

	/**
	 * {@link ObjectMapper}.
	 */
	private final ObjectMapper mapper = new ObjectMapper();

	/**
	 * {@link ArrayType} for parsing the JSON.
	 */
	private final ArrayType arrayType = this.mapper.getTypeFactory()
			.constructArrayType(Result.class);

	/**
	 * Number of items to request.
	 */
	private volatile int requestBatchIndex = 0;

	/**
	 * Listing of request batch sizes.
	 */
	private final String[] queryValues = new String[] { null, "INVALID", "0",
			"1", "5", "10", "15", "20", "500", "501" };

	/**
	 * Corresponding list of the resulting request batch size.
	 */
	private final int[] requestBatchSizes = new int[] { 1, 1, 1, 1, 5, 10, 15,
			20, 500, 501 };

	@Data
	public static class Result {
		private int id;
		private int randomNumber;
	}

	@Test
	public void ensureReadJson() throws IOException {
		String entity = "[{\"id\":4174,\"randomNumber\":331}"
				+ ",{\"id\":51,\"randomNumber\":6544}"
				+ ",{\"id\":4462,\"randomNumber\":952}"
				+ ",{\"id\":2221,\"randomNumber\":532}"
				+ ",{\"id\":9276,\"randomNumber\":3097}"
				+ ",{\"id\":3056,\"randomNumber\":7293}"
				+ ",{\"id\":6964,\"randomNumber\":620}"
				+ ",{\"id\":675,\"randomNumber\":6601}"
				+ ",{\"id\":8414,\"randomNumber\":6569}"
				+ ",{\"id\":2753,\"randomNumber\":4065}]";
		Result[] results = (Result[]) this.mapper.readValue(entity,
				this.arrayType);
		Assert.assertEquals("Incorrect number of elements", 10, results.length);
		assertResult(results[0], 4174, 331);
		assertResult(results[1], 51, 6544);
		assertResult(results[2], 4462, 952);
		assertResult(results[3], 2221, 532);
		assertResult(results[4], 9276, 3097);
		assertResult(results[5], 3056, 7293);
		assertResult(results[6], 6964, 620);
		assertResult(results[7], 675, 6601);
		assertResult(results[8], 8414, 6569);
		assertResult(results[9], 2753, 4065);
	}

	private static void assertResult(Result result, int id, int randomNumber) {
		Assert.assertEquals("Incorrect identifier", id, result.getId());
		Assert.assertEquals("Incorrect random number", randomNumber,
				result.getRandomNumber());
	}

	@Override
	protected void doRequestTest(CloseableHttpClient client) throws Exception {

		// Obtain the next index
		int index = this.requestBatchIndex;

		// Provide the index for next request (duplicate batch sizes ok)
		this.requestBatchIndex = (index + 1) % this.requestBatchSizes.length;

		// Obtain the query string
		String queryString = this.queryValues[index];
		queryString = (queryString == null ? "" : "?queries=" + queryString);

		// Obtain the expected batch size
		int batchSize = this.requestBatchSizes[index];

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
		Result[] results = (Result[]) this.mapper.readValue(entity,
				this.arrayType);
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