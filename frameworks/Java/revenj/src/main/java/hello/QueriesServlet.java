package hello;

import com.dslplatform.json.JsonWriter;
import dsl.FrameworkBench.World;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;

public class QueriesServlet extends HttpServlet {

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
		final int count = Utils.parseBoundParam(req);
		final Context ctx = Utils.getContext();
		final JsonWriter json = ctx.json;
		final World[] worlds = ctx.loadWorldsSlow(count);
		json.serialize(worlds, count);
		json.toStream(res.getOutputStream());
	}
}
