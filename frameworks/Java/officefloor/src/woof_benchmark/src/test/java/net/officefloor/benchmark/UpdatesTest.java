package net.officefloor.benchmark;

import static org.junit.Assert.assertEquals;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.Data;
import net.officefloor.jdbc.test.DataSourceRule;
import net.officefloor.server.http.mock.MockHttpResponse;
import net.officefloor.server.http.mock.MockHttpServer;
import net.officefloor.woof.mock.MockWoofServerRule;

/**
 * Tests multiple queries.
 */
public class UpdatesTest {

	@ClassRule
	public static DataSourceRule dataSource = new DataSourceRule("datasource.properties");

	@Rule
	public MockWoofServerRule server = new MockWoofServerRule();

	@Before
	public void setupDatabase() throws SQLException {
		try (Connection connection = dataSource.getConnection()) {
			connection.createStatement().executeUpdate("CREATE TABLE World ( id INT PRIMARY KEY, randomNumber INT)");
			PreparedStatement insert = connection
					.prepareStatement("INSERT INTO World (id, randomNumber) VALUES (?, ?)");
			for (int i = 0; i < 10000; i++) {
				insert.setInt(1, i + 1);
				insert.setInt(2, ThreadLocalRandom.current().nextInt(1, 10000));
				insert.executeUpdate();
			}
		}
	}

	@Test
	public void validRequest() throws Exception {
		MockHttpResponse response = this.server.send(MockHttpServer.mockRequest("/updates?queries=10"));
		assertEquals("Should be successful", 200, response.getStatus().getStatusCode());
		response.assertHeader("content-type", "application/json");
		WorldResponse[] worlds = new ObjectMapper().readValue(response.getEntity(null), WorldResponse[].class);
		assertEquals("Incorrect number of worlds", 10, worlds.length);
	}

	@Data
	public static class WorldResponse {
		private int id;
		private int randomNumber;
	}

}