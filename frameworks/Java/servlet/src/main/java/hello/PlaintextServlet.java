package hello;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Plaintext rendering Test
 */
@SuppressWarnings("serial")
public class PlaintextServlet extends HttpServlet {
	private static final String MESSAGE = "Hello, World!";
	private static final byte[] buffer;

	static {
		try {
			buffer = MESSAGE.getBytes("US-ASCII");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		// Set content type to text/plain.
		res.setContentType(Common.CONTENT_TYPE_TEXT);

		// Write plaintext "Hello, World!" to the response.
		res.getOutputStream().write(buffer);
	}

}
