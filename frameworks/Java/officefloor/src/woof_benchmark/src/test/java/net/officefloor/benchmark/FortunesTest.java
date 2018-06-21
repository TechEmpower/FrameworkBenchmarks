package net.officefloor.benchmark;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;

import net.officefloor.jdbc.test.DataSourceRule;
import net.officefloor.server.http.mock.MockHttpResponse;
import net.officefloor.server.http.mock.MockHttpServer;
import net.officefloor.woof.mock.MockWoofServerRule;

/**
 * Tests multiple queries.
 */
public class FortunesTest {

	@ClassRule
	public static DataSourceRule dataSource = new DataSourceRule("datasource.properties");

	@Rule
	public MockWoofServerRule server = new MockWoofServerRule();

	@Before
	public void setupDatabase() throws SQLException {
		try (Connection connection = dataSource.getConnection()) {
			connection.createStatement()
					.executeUpdate("CREATE TABLE Fortune ( id INT PRIMARY KEY, message VARCHAR(100))");
			PreparedStatement insert = connection.prepareStatement("INSERT INTO Fortune (id, message) VALUES (?, ?)");
			int id = 1;
			for (String message : new String[] { "fortune: No such file or directory",
					"A computer scientist is someone who fixes things that aren't broken.",
					"After enough decimal places, nobody gives a damn.",
					"A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1",
					"A computer program does what you tell it to do, not what you want it to do.",
					"Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen",
					"Any program that runs right is obsolete.",
					"A list is only as strong as its weakest link. — Donald Knuth", "Feature: A bug with seniority.",
					"Computers make very fast, very accurate mistakes.",
					"<script>alert(\"This should not be displayed in a browser alert box.\");</script>",
					"フレームワークのベンチマーク" }) {
				insert.setInt(1, id++);
				insert.setString(2, message);
				insert.executeUpdate();
			}
		}
	}

	@Test
	public void validRequest() throws Exception {
		MockHttpResponse response = this.server.send(MockHttpServer.mockRequest("/fortunes"));
		response.assertResponse(200,
				"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
						+ "<tr><td>11</td><td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;</td></tr>"
						+ "<tr><td>4</td><td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td></tr>"
						+ "<tr><td>5</td><td>A computer program does what you tell it to do, not what you want it to do.</td></tr>"
						+ "<tr><td>2</td><td>A computer scientist is someone who fixes things that aren't broken.</td></tr>"
						+ "<tr><td>8</td><td>A list is only as strong as its weakest link. &mdash; Donald Knuth</td></tr>"
						+ "<tr><td>0</td><td>Additional fortune added at request time.</td></tr>"
						+ "<tr><td>3</td><td>After enough decimal places, nobody gives a damn.</td></tr>"
						+ "<tr><td>7</td><td>Any program that runs right is obsolete.</td></tr>"
						+ "<tr><td>10</td><td>Computers make very fast, very accurate mistakes.</td></tr>"
						+ "<tr><td>6</td><td>Emacs is a nice operating system, but I prefer UNIX. &mdash; Tom Christaensen</td></tr>"
						+ "<tr><td>9</td><td>Feature: A bug with seniority.</td></tr>"
						+ "<tr><td>1</td><td>fortune: No such file or directory</td></tr>"
						+ "<tr><td>12</td><td>フレームワークのベンチマーク</td></tr>" + "</table></body></html>");
		response.assertHeader("content-type", "text/html");
	}

}