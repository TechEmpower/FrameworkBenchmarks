package net.officefloor.benchmark;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.Data;
import net.officefloor.jdbc.test.DataSourceRule;
import net.officefloor.server.http.HttpClientRule;
import net.officefloor.server.http.HttpServer;
import net.officefloor.server.http.HttpServerLocation;
import net.officefloor.server.http.SystemPropertiesRule;
import net.officefloor.test.OfficeFloorRule;

/**
 * Tests multiple queries.
 */
public class UpdateTest {

	@ClassRule
	public static DataSourceRule dataSource = new DataSourceRule("datasource.properties");

	@ClassRule
	public static SystemPropertiesRule systemProperties = new SystemPropertiesRule(HttpServer.PROPERTY_HTTP_SERVER_NAME,
			"OF", HttpServer.PROPERTY_HTTP_DATE_HEADER, "true", HttpServerLocation.PROPERTY_HTTP_PORT, "8080");

	@Rule
	public OfficeFloorRule server = new OfficeFloorRule();

	@Rule
	public HttpClientRule client = new HttpClientRule();

	@Before
	public void setupDatabase() throws SQLException {
		try (Connection connection = dataSource.getConnection()) {
			try {
				connection.createStatement().executeQuery("SELECT * FROM World");
			} catch (SQLException ex) {
				connection.createStatement()
						.executeUpdate("CREATE TABLE World ( id INT PRIMARY KEY, randomNumber INT)");
				PreparedStatement insert = connection
						.prepareStatement("INSERT INTO World (id, randomNumber) VALUES (?, ?)");
				for (int i = 0; i < 10000; i++) {
					insert.setInt(1, i + 1);
					insert.setInt(2, ThreadLocalRandom.current().nextInt(1, 10000));
					insert.executeUpdate();
				}
			}
		}
	}

	@Test
	public void validRequest() throws Exception {
		HttpResponse response = this.client.execute(new HttpGet("http://localhost:8080/update?queries=20"));
		assertEquals("Should be successful", 200, response.getStatusLine().getStatusCode());
		assertEquals("Incorrect content-type", "application/json", response.getFirstHeader("content-type").getValue());
		assertEquals("Incorrect server", "OF", response.getFirstHeader("Server").getValue());
		assertNotNull("Should have date", response.getFirstHeader("date"));
		WorldResponse[] worlds = new ObjectMapper().readValue(EntityUtils.toString(response.getEntity()),
				WorldResponse[].class);
		assertEquals("Incorrect number of worlds", 20, worlds.length);

		// Ensure database updated to the values
		try (Connection connection = dataSource.getConnection()) {
			PreparedStatement statement = connection
					.prepareStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = ?");
			for (WorldResponse world : worlds) {
				statement.setInt(1, world.getId());
				ResultSet resultSet = statement.executeQuery();
				assertTrue("Should find row", resultSet.next());
				assertEquals("Should update row " + world.getId(), world.getRandomNumber(), resultSet.getInt(2));
			}
		}
	}

	@Data
	public static class WorldResponse {
		private int id;
		private int randomNumber;
	}

}