package hello;

import dsl.FrameworkBench.Fortune;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.*;

public class FortunesServlet extends HttpServlet {

	private static final Comparator<Fortune> COMPARATOR = (o1, o2) -> o1.getMessage().compareTo(o2.getMessage());

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		final Context ctx = Utils.getContext();
		final List<Fortune> fortunes = ctx.fortunes.search();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		Collections.sort(fortunes, COMPARATOR);
		req.setCharacterEncoding("UTF-8");
		req.setAttribute("fortunes", fortunes);
		req.getRequestDispatcher("/WEB-INF/jsp/fortunes.jsp").forward(req, res);
	}
}
