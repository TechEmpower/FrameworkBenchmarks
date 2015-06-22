package dsl;

import java.io.*;
import java.nio.charset.Charset;

import javax.servlet.*;
import javax.servlet.http.*;

public class PlaintextServlet extends HttpServlet {
	private static final Charset utf8 = Charset.forName("UTF-8");
	private static final byte[] HELLO_WORLD = "Hello, World!".getBytes(utf8);

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("text/plain");
		res.getOutputStream().write(HELLO_WORLD);
	}
}
