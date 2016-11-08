package hello;

import com.dslplatform.json.JsonWriter;
import dsl.FrameworkBench.World;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.*;

public class UpdatesServlet extends HttpServlet {

	private static final Comparator<World> ASC = (l, r) -> l.getId() - r.getId();

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		res.setContentType("application/json");
		final int count = Utils.parseBoundParam(req);
		final Context ctx = Utils.getContext();
		final JsonWriter json = ctx.json;
		final World[] worlds = ctx.loadWorldsSlow(count);
		final ArrayList<World> changed = new ArrayList<>(count);
		for (int i = 0; i < count; i++) {
			changed.add(worlds[i].setRandomNumber(ctx.getRandom10k()));
		}
		Collections.sort(changed, ASC);
		ctx.worlds.update(changed);
		json.serialize(worlds, count);
		json.toStream(res.getOutputStream());
	}
}