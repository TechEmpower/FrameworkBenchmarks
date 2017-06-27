package hello;

import com.fasterxml.jackson.databind.*;

/**
 * Some common functionality and constants used by the Servlet tests.
 */
public class Common {
	// Constants for setting the content type.
	protected static final String HEADER_CONTENT_TYPE = "Content-Type";
	protected static final String CONTENT_TYPE_JSON = "application/json";
	protected static final String CONTENT_TYPE_TEXT = "text/plain";
	protected static final String CONTENT_TYPE_HTML = "text/html";

	// Jackson encoder, reused for each response.
	protected static final ObjectMapper MAPPER = new ObjectMapper();
}