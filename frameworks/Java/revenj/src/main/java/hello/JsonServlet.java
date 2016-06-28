package hello;

import com.dslplatform.json.JsonWriter;
import dsl.FrameworkBench.Message;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class JsonServlet extends HttpServlet {
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
		final Message msg = new Message("Hello, World!");
		final JsonWriter writer = Utils.getContext().json;
		msg.serialize(writer, false);
		writer.toStream(res.getOutputStream());
	}
}
