package net.officefloor.benchmark;

import org.junit.Rule;
import org.junit.Test;

import net.officefloor.server.http.mock.MockHttpResponse;
import net.officefloor.server.http.mock.MockHttpServer;
import net.officefloor.woof.mock.MockWoofServerRule;

/**
 * Tests JSON.
 */
public class JsonTest {

	@Rule
	public MockWoofServerRule server = new MockWoofServerRule();

	@Test
	public void validRequest() {
		MockHttpResponse response = this.server.send(MockHttpServer.mockRequest("/json"));
		response.assertResponse(200, "{\"message\":\"Hello, World!\"}", "content-type", "application/json");
	}

}
