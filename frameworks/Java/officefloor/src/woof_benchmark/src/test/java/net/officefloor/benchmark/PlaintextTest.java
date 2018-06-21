package net.officefloor.benchmark;

import org.junit.Rule;
import org.junit.Test;

import net.officefloor.server.http.mock.MockHttpResponse;
import net.officefloor.server.http.mock.MockHttpServer;
import net.officefloor.woof.mock.MockWoofServerRule;

/**
 * Tests the plain text.
 */
public class PlaintextTest {

	@Rule
	public MockWoofServerRule server = new MockWoofServerRule();

	@Test
	public void validRequest() {
		MockHttpResponse response = this.server.send(MockHttpServer.mockRequest("/plaintext"));
		response.assertResponse(200, "Hello, World!", "content-type", "text/plain");
	}

}
