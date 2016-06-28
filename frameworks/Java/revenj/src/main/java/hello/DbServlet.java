package hello;

import com.dslplatform.json.JsonWriter;
import dsl.FrameworkBench.World;

import java.io.*;
import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class DbServlet extends HttpServlet {

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
		final Context ctx = Utils.getContext();
		final Optional<World> world = ctx.repository.find(ctx.getRandom10k(), ctx.connection);
		final JsonWriter writer = ctx.json;
		world.get().serialize(writer, false);
		writer.toStream(res.getOutputStream());
	}
}
