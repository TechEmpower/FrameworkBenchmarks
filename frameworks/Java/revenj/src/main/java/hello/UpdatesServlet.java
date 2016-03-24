package hello;

import com.dslplatform.json.JsonWriter;
import dsl.FrameworkBench.World;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.ArrayList;

public class UpdatesServlet extends HttpServlet {

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
		final int count = Utils.parseBoundParam(req);
		final Context ctx = Utils.getContext();
		final JsonWriter json = ctx.json;
		ctx.loadWorlds(count);
		final World[] worlds = ctx.worlds;
		final ArrayList<World> changed = new ArrayList<>(count);
		for (int i = 0; i < count; i++) {
			changed.add(worlds[i].setRandomNumber(ctx.getRandom10k()));
		}
		ctx.repository.update(changed);
		json.serialize(worlds, count);
		json.toStream(res.getOutputStream());
	}
}
