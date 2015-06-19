package dsl;

import com.dslplatform.client.json.JsonWriter;
import dsl.Bench.Message;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class JsonServlet extends HttpServlet {
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
		final Message msg = new Message("Hello, World!");
		final JsonWriter writer = Utils.getJson();
		msg.serialize(writer, false);
		writer.toStream(res.getOutputStream());
	}
}
